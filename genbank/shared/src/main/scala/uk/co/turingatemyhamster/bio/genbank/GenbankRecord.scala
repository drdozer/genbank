package uk.co.turingatemyhamster.bio.genbank

case class GenbankHeader(fileName: String,
                         databaseName: String,
                         releaseDate: String,
                         releaseNumber: String,
                         title: String,
                         entries: Int, bases: Long, reports: Int)

/**
 * See: ftp://ftp.ncbi.nih.gov/genbank/gbrel.txt
 * See: http://www.insdc.org/files/feature_table.html
 *
 * @author Matthew Pocock
 */
case class GenbankRecord(locus: Locus,
                         definition: Definition,
                         accession: Accession,
                         version: Version,
                         dbLink: Option[DbLink],
                         keywords: Keywords,
                         segment: Option[Segment],
                         source: Source,
                         references: Seq[Reference],
                         comment: Option[Comment],
                         features: Seq[Feature],
                         seqOrContig: Either[(Origin, Sequence), Contig])

case class Locus(locusName: String,
                 sequenceLength: Int,
                 moleculeType: String,
                 moleculeTopology: String,
                 genbankDivision: String,
                 modificationDate: String)

case class Definition(value: String)

case class Accession(values: Seq[String])

case class Version(accessionNumber: String, giIdentifier: String)

case class DbLink(links: Seq[Link])
case class Link(crossRefType: String, crossRefIds: Seq[String])

case class Keywords(values: Seq[String])

case class Segment(n: Int, total: Int)

case class Source(value: String, organism: Organism)

case class Organism(name: String, values: Seq[String])

case class Reference(number: Int,
                     bases: Seq[BaseRange],
                     authors: Option[Seq[Author]],
                     consortium: Seq[Consortium],
                     title: Option[String],
                     journal: String,
                     medline: Option[Int],
                     pubmed: Option[Int],
                     remark: Option[String])

case class Author(name: Option[String], initials: Seq[String], numeral: Option[String])

case class BaseRange(from: Int, to: Int)

case class Consortium(name: String)

case class Comment(lines: Seq[String])

case class Feature(key: String,
                   location: Location,
                   annotations: Seq[Qualifier])

sealed trait Location

object Location {
  sealed trait Position extends Location
  sealed trait Operation extends Location
  sealed trait Extent extends Position
  sealed trait Point extends Position

  case class SingleBase(at: Int) extends Point
  case class AtOrBefore(at: SingleBase) extends Point
  case class AtOrAfter(at: SingleBase) extends Point
  case class NestedOneOf(oneOf: OneOf) extends Point

  case class OneOf(from: SingleBase, to: SingleBase) extends Extent
  case class Cut(from: SingleBase, to: SingleBase) extends Extent
  case class Span(from: Point, to: Point) extends Extent
  case class RemoteReference(withinEntry: String, remoteLocation: Position) extends Location

  case class Complement(of: Location) extends Operation
  case class Join(subLocations: Seq[Location]) extends Operation
  case class Order(subLocations: Seq[Location]) extends Operation
}

sealed trait QualifierValue
case class QuotedValue(value: String) extends QualifierValue
case class UnquotedValue(value: String) extends QualifierValue

case class Qualifier(key: String, value: Option[QualifierValue])

case class Origin(value: Option[String])

case class Sequence(value: String)

case class Contig(joined: Seq[ContigPart])

sealed trait ContigPart

object ContigPart {
  case class Complement(of: RemoteReference) extends ContigPart
  case class RemoteReference(withinEntry: String, remoteLocation: Span) extends ContigPart
  case class Span(from: SingleBase, to: SingleBase) extends ContigPart
  case class SingleBase(at: Int)

  sealed trait Gap extends ContigPart
  object UnknownGap extends Gap
  case class KnownGap(size: Int) extends Gap
  case class UnknownNs(size: Int) extends Gap
}