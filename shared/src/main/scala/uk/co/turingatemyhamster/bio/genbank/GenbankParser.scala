package uk.co.turingatemyhamster.bio.genbank

import org.parboiled2._

/**
 *
 *
 * @author Matthew Pocock
 */
trait GenbankParser extends Parser {
  // boilerplate
  type Emit[T] = T => Appendable => Unit

  implicit class EmitFusion(val _app: Appendable => Unit) extends AnyVal {
    def ~ (app2: Appendable => Unit): Appendable => Unit = a => {
      _app(a)
      app2(a)
    }
  }

  // grammar starts here
  def R_GenbankRecords = rule { zeroOrMore(R_GenbankRecord) }


  // GenbankRecord
  type T_GenbankRecord

  trait T_GenbankRecord_Companion {
    def apply(locus: T_Locus,
              description: T_Description,
              accession: T_Accession,
              version: T_Version,
              keywords: T_Keywords,
              source: T_Source,
              references: T_References,
              features: T_Features,
              origin: T_Origin,
              sequence: T_Sequence): T_GenbankRecord
    def unapply(gr: T_GenbankRecord): Option[
      (T_Locus,
        T_Description,
        T_Accession,
        T_Version,
        T_Keywords,
        T_Source,
        T_References,
        T_Features,
        T_Origin,
        T_Sequence)]
  }
  
  val T_GenbankRecord: T_GenbankRecord_Companion

  def R_GenbankRecord: Rule1[T_GenbankRecord] = rule {
    R_Locus ~
      R_Description ~
      R_Accession ~
      R_Version ~
      R_Keywords ~
      R_Source ~
      R_References ~
      R_Features ~
      R_Origin ~
      R_Sequence ~
      R_RecordSeparator -> (T_GenbankRecord apply)
  }

  def E_GenbankRecord: Emit[T_GenbankRecord] = {
    case T_GenbankRecord(locus,
                         description,
                         accession,
                         version,
                         keywords,
                         source,
                         references,
                         features,
                         origin,
                         sequence) =>
      E_Locus(locus) ~
      E_Description(description) ~
      E_Accession(accession) ~
      E_Version(version) ~
      E_Keywords(keywords) ~
      E_Source(source) ~
      E_References(references) ~
      E_Features(features) ~
      E_Origin(origin) ~
      E_Sequence(sequence) ~
      E_RecordSeparator
  }


  // Locus
  type T_Locus

  trait T_Locus_Companion {
    def apply(locusName: String,
              sequenceLength: String,
              moleculeType: String,
              genbankDivision: String,
              modificationDate: String): T_Locus
    def unapply(l: T_Locus): Option[(String, String, String, String, String)]
  }
  
  val T_Locus: T_Locus_Companion

  def R_Locus = rule {
    R_LocusName ~
      R_SequenceLength ~
      R_MoleculeType ~
      R_GenbankDivision ~
      R_ModificationDate ->
    (T_Locus apply)
  }

  def E_Locus: Emit[T_Locus] = {
    case T_Locus(locusName, sequenceLength, moleculeType, genbankDivision, modificationDate) =>
      E_LocusName ~
      E_SequenceLength ~
      E_MoleculeType ~
      E_GenbankDivision ~
      E_ModificationDate
  }


  // Description
  type T_Description

  trait T_Description_Companion {
    def apply(value: String): Description
    def unapply(d: Description): Option[String]
  }

  val T_Description: T_Description_Companion

  def R_Locus = rule {
    keyword("DESCRIPTION") ~ capture(String)
  }

  type T_Accession
  type T_Version
  type T_Keywords
  type T_Source
  type T_References
  type T_Features
  type T_Origin
  type T_Sequence
  

}
