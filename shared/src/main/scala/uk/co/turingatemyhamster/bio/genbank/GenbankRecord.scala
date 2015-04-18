package uk.co.turingatemyhamster.bio.genbank

/**
 *
 *
 * @author Matthew Pocock
 */
case class GenbankRecord(locus: Locus,
                         definition: Description,
                         accession: Accession,
                         version: Version,
                         keywords: Keywords,
                         source: Source,
                         references: Seq[Reference],
                         features: Seq[Feature],
                         origin: Origin,
                         sequence: Sequence)

case class Locus(locusName: String,
                 sequenceLength: String,
                 moleculeType: String,
                 genbankDivision: String,
                 modificationDate: String)

case class Description(value: String)

case class Accession(value: String)

case class Version(value: String)

case class Keywords(values: Seq[String])

case class Source(value: String, organism: Organism)

case class Organism(values: Seq[String])

case class Reference(number: Int,
                     bases: (Int, Int),
                     authors: Seq[Author],
                     title: String,
                     journal: String,
                     pubmed: Option[String],
                     directSubmission: Option[String])

case class Author(value: String)

case class Feature(key: String,
                   location: Location,
                   annotations: Seq[Qualifier])

sealed trait Location
sealed trait Position extends Location
sealed trait Operation extends Location

case class SingleBase(at: Int) extends Position
case class Span(from: Int, to: Int) extends Position
case class Between(span: Span) extends Position
case class OneBaseWithin(span: Span) extends Position
case class OneBaseOf(bases: Seq[SingleBase]) extends Position
case class RemoteReference(withinEntry: String, remoteLocation: Position)
case class Complement(of: Location)
case class Join(subLocations: Seq[Location])
case class Order(subLocations: Seq[Location])

sealed trait QualifierValue
case class TextValue(value: String) extends QualifierValue
case class NumericValue(value: Int) extends QualifierValue
case class CitationNumber(value: Int) extends QualifierValue

case class Qualifier(key: String, value: QualifierValue)

case class Origin(value: Option[String])

case class Sequence(value: String)