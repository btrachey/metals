package scala.meta.internal.metals.signature

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.meta.internal.metals.BuildTargets
import scala.meta.internal.metals.SemanticdbFeatureProvider
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.internal.semanticdb.XtensionSemanticdbSymbolInformation
import scala.meta.io.AbsolutePath

import org.virtuslab.inkuire.engine.api.InkuireDb
import org.virtuslab.inkuire.engine.api.InkuireEnv
import org.virtuslab.inkuire.engine.impl.model.{
  AnnotatedSignature => InkuireSignature
}
import org.eclipse.{lsp4j => l}

/**
 * Searches methods and functions by their type signature.
 *
 * Both workspace and dependency (jar) methods are indexed into a single
 * [[org.virtuslab.inkuire.engine.api.InkuireDb]] and searched with Inkuire's
 * own query engine
 * (https://github.com/VirtusLab/Inkuire) — the same Hoogle-like,
 * type-directed search Scala 3's Scaladoc uses.
 *
 *  - **Workspace methods/values** are indexed from compiled SemanticDB
 *    (see [[InkuireDbBuilder]]) and carry full type information.
 *  - **Dependency (jar) methods** are indexed by parsing `.class` files
 *    (see [[JarInkuireDbBuilder]]). Generic signatures are recovered from
 *    the JVM `Signature` attribute when present; otherwise the erased
 *    descriptor is used. Type parameters are treated as invariant.
 *
 * A query describes a signature shape (arrows, generics, tuples,
 * `A | B`/`A & B`, `_` wildcards, `[A, B] => ...` type variables,
 * `+pkg`/`-pkg` package filters) and matches up to subtyping/variance.
 * Methods are indexed without a receiver, so a query lists just the argument
 * types and result — `User => Boolean => Box[MemberDetailsSummary]` finds a
 * method taking a `User` and a `Boolean` and returning `Box[MemberDetailsSummary]`,
 * regardless of which class/trait it is declared in. Top-level commas are
 * accepted as argument separators and rewritten to `=>` (`User, Boolean => ...
 * ` is treated as `User => Boolean => ...`); commas inside `(...)`/`[...]`
 * are left alone as tuples / type arguments.
 */
final class SignatureSearchProvider(
    buildTargets: BuildTargets,
    workspace: AbsolutePath,
)(implicit ec: ExecutionContext)
    extends SemanticdbFeatureProvider {

  // --- workspace index: real Inkuire data, built from SemanticDB --------------

  /** Per-source-file contribution to the workspace `InkuireDb`. */
  private val docDbs = TrieMap.empty[Path, InkuireDb]

  private case class SymbolLocation(
      kind: l.SymbolKind,
      path: AbsolutePath,
      range: Option[l.Range],
  )

  /** SemanticDB symbol (== `AnnotatedSignature.uri`, see [[InkuireDbBuilder]]) -> where to point the LSP result. */
  private val symbolLocations = TrieMap.empty[String, SymbolLocation]
  private val pathToSymbols: TrieMap[Path, mutable.Set[String]] = TrieMap.empty

  // --- dependency (jar) index: Inkuire data, built from class files -----------

  private val jarDb = new AtomicReference[InkuireDb](InkuireDb.empty)

  /**
   * Maps `AnnotatedSignature.uri` (for jar methods) to the jar `AbsolutePath`
   * that declared the method, so we can build an LSP location for the result.
   */
  private val jarLocations = TrieMap.empty[String, AbsolutePath]

  /** Bumped on every workspace index change; invalidates the cached [[InkuireEnv]]. */
  private val version = new AtomicInteger(0)
  private val envCache =
    new AtomicReference[(Int, InkuireEnv)]((-1, null))

  private def currentEnv(): InkuireEnv = {
    val v = version.get()
    envCache.get() match {
      case (cachedV, env) if cachedV == v && env != null => env
      case _ =>
        // `withOrphanTypes` registers a stub `types` entry (name + itid, no known
        // parents) for every type mentioned in a signature but not itself defined
        // — without it, Inkuire can't resolve a query that mentions any such type.
        val workspaceDb =
          InkuireDbBuilder.combineAll(docDbs.values).withOrphanTypes
        val combined =
          InkuireDbBuilder
            .combineAll(
              Seq(workspaceDb, jarDb.get()).filterNot(_.functions.isEmpty)
            )
            .withOrphanTypes
        val env = InkuireEnv.defaultScalaEnv()(combined)
        envCache.set((v, env))
        env
    }
  }

  override def onChange(docs: TextDocuments, path: AbsolutePath): Unit =
    indexWorkspaceDocuments(docs, path)

  override def onDelete(path: AbsolutePath): Unit =
    removeWorkspacePath(path.toNIO)

  override def reset(): Unit = {
    docDbs.clear()
    symbolLocations.clear()
    pathToSymbols.clear()
    version.incrementAndGet()
  }

  /**
   * Search by a raw query string.
   *
   * An empty query returns every indexed method so a downstream UI can do its
   * own fuzzy filtering over the full result set.
   */
  def search(query: String): Future[List[l.SymbolInformation]] = {
    val trimmed = query.trim
    if (trimmed.isEmpty) {
      Future.successful(allResults())
    } else {
      // Rewrite top-level commas into `=>` so the natural query shape
      // `User, Boolean => Box[T]` is accepted as `User => Boolean => Box[T]`.
      // Inkuire's grammar only accepts commas inside `( … )` tuples or `[ … ]`
      // type arguments, so a top-level comma always fails to parse; rewriting
      // it is a safe, lossless convenience.
      val normalized = normalizeTopLevelCommas(trimmed)
      Future.successful(queryInkuire(normalized))
    }
  }

  /**
   * Replace commas that sit at nesting depth 0 (i.e. outside any `(` `)` or
   * `[` `]` pair) with ` => `. Commas inside parens/brackets are preserved:
   * they denote either a tuple element (`(A, B)`) or a type argument (`Box[A, B]`),
   * both of which Inkuire parses natively.
   */
  private def normalizeTopLevelCommas(query: String): String = {
    val out = new StringBuilder(query.length)
    var depth = 0
    var i = 0
    while (i < query.length) {
      val c = query.charAt(i)
      c match {
        case '(' | '[' =>
          depth += 1
          out.append(c)
        case ')' | ']' =>
          depth = math.max(0, depth - 1)
          out.append(c)
        case ',' if depth == 0 =>
          out.append(" => ")
        case _ =>
          out.append(c)
      }
      i += 1
    }
    out.toString
  }

  /**
   * Best-effort scan of the class files in the given jars, building an
   * [[InkuireDb]] that can be merged with the workspace index. Jars are
   * read directly, so no embedded SemanticDB is required.
   */
  def scanJars(jars: Iterable[AbsolutePath]): Unit = {
    val (db, locations) =
      JarInkuireDbBuilder.build(jars.map(_.toNIO))
    jarDb.set(db)
    jarLocations.clear()
    locations.foreach { case (uri, jarPath) =>
      jarLocations(uri) = AbsolutePath(jarPath)
    }
  }

  // --- unified query ---------------------------------------------------------

  private def allResults(): List[l.SymbolInformation] = {
    val env = currentEnv()
    val db = env.db
    (db.functions).flatMap(toLsp).toList
  }

  private def queryInkuire(query: String): List[l.SymbolInformation] =
    currentEnv().query(query) match {
      case Right(matches) =>
        matches.flatMap(toLsp).toList
      case Left(error) =>
        scribe.warn(
          s"Signature search: could not parse/resolve query '$query': $error"
        )
        Nil
    }

  private def toLsp(sig: InkuireSignature): Option[l.SymbolInformation] = {
    // Workspace symbols first, then jar methods.
    // For workspace symbols we require a valid source range: without one the
    // result would point to (0,0) which is useless for navigation and usually
    // indicates a stale or synthetic semanticdb entry (e.g. a method that was
    // moved to another file but the old semanticdb hasn't been regenerated yet).
    val workspaceLoc = symbolLocations
      .get(sig.uri)
      .filter(_.range.isDefined)
    val loc = workspaceLoc.orElse(
      jarLocations
        .get(sig.uri)
        .map { jarPath =>
          SymbolLocation(
            kind = l.SymbolKind.Method,
            path = jarPath,
            range = None,
          )
        }
    )
    loc.map(toLsp(sig, _))
  }

  private def toLsp(
      sig: InkuireSignature,
      loc: SymbolLocation,
  ): l.SymbolInformation = {
    val range =
      loc.range.getOrElse(
        new l.Range(new l.Position(0, 0), new l.Position(0, 0))
      )
    new l.SymbolInformation(
      sig.name,
      loc.kind,
      new l.Location(loc.path.toURI.toString, range),
      sig.packageName,
    )
  }

  /** Index a workspace source from its (fully-typed) SemanticDB document. */
  private def indexWorkspaceDocuments(
      docs: TextDocuments,
      sourcePath: AbsolutePath,
  ): Unit = {
    val key = sourcePath.toNIO
    removeWorkspacePath(key)

    val defRanges: Map[String, Option[l.Range]] = docs.documents
      .flatMap(_.occurrences)
      .collect {
        case occ if occ.role.isDefinition =>
          occ.symbol -> occ.range.map(_.toLsp)
      }
      .toMap

    // Compiler-generated case-class members (`copy$default$N`, `apply`,
    // `unapply`, `toString`, `hashCode`, `equals`, `canEqual`, `productXxx`, ...)
    // aren't reliably marked SYNTHETIC by semanticdb-scalac, but none of them
    // have a definition occurrence — there's no source token to point at. Drop
    // any function lacking one: it can't be given a meaningful location anyway
    // (see `toLsp`'s (0,0) fallback), and it's compiler noise, not something
    // worth surfacing in search results.
    val definedSymbols: Set[String] =
      docs.documents
        .flatMap(_.occurrences)
        .collect {
          case occ if occ.role.isDefinition => occ.symbol
        }
        .toSet

    val rawFileDb =
      InkuireDbBuilder.combineAll(
        docs.documents.map(InkuireDbBuilder.fromTextDocument)
      )
    val fileDb = rawFileDb.copy(
      functions =
        rawFileDb.functions.filter(f => definedSymbols.contains(f.uri))
    )
    if (fileDb.functions.nonEmpty) docDbs(key) = fileDb

    // Register locations only for symbols that are actually in the Inkuire
    // DB for this file. Using `doc.symbols` directly would also register
    // locations for inherited or compiler-generated symbols that passed the
    // `definedSymbols` check but were filtered out of `fileDb.functions` by
    // `isVisible` (non-public, synthetic, override, etc.). Worse, scalac can
    // emit definition occurrences for inherited members in the inheriting
    // file's semanticdb, causing the global `symbolLocations` map to point
    // to the wrong file. Filtering by `fileDb.functions` keeps the location
    // map consistent with what Inkuire can actually return.
    val indexedSymbols: Set[String] =
      fileDb.functions.view.map(_.uri).toSet
    val symbolKinds: Map[String, l.SymbolKind] =
      docs.documents
        .flatMap(_.symbols)
        .collect {
          case info if info.isMethod || info.isConstructor || info.isField =>
            info.symbol -> info.kind.toLsp
        }
        .toMap

    val seen = mutable.Set.empty[String]
    for {
      sym <- indexedSymbols
      kindOpt <- symbolKinds.get(sym)
      range <- defRanges.get(sym).flatten
    } {
      // Only register locations where we have a concrete source range.
      // Without a range the result would point to (0,0), which is
      // useless for navigation and typically indicates a stale or
      // synthetic semanticdb entry.
      //
      // Also: don't clobber an existing entry that already has a valid
      // range. If two files' semanticdb both claim a symbol (which can
      // happen with stale artifacts), the first one with a real range
      // wins instead of last-write-wins.
      symbolLocations.get(sym) match {
        case Some(existing) if existing.range.isDefined => ()
        case _ =>
          seen += sym
          symbolLocations(sym) = SymbolLocation(
            kind = kindOpt,
            path = sourcePath,
            range = Some(range),
          )
      }
    }
    if (seen.nonEmpty) {
      val existing = pathToSymbols.getOrElseUpdate(key, mutable.Set.empty)
      existing.synchronized { seen.foreach(existing += _) }
    }
    version.incrementAndGet()
  }

  private def removeWorkspacePath(path: Path): Unit = {
    docDbs.remove(path)
    pathToSymbols.remove(path).foreach(_.foreach(symbolLocations.remove))
    version.incrementAndGet()
  }
}
