package uk.co.turingatemyhamster.bio
package genbank

import fastparse.all._
import fastparse.core.Mutable.Success



/**
 * See: ftp://ftp.ncbi.nih.gov/genbank/gbrel.txt
 * See: http://www.insdc.org/files/feature_table.html
 *
 * @author Matthew Pocock
 */
object GenbankParser {

  val newline = "\r\n" | "\r" | "\n"
  val space = " "
  val period = "."
  val comma = ","
  val hyphen = "-"
  val spaces = space.rep(1)
  val spaces_? = space.rep
  val notSpace = !" "~AnyChar
  val notSpaces = notSpace.rep(1)
  val notNewline = !newline~AnyChar
  val notNewlines = notNewline.rep(1)
  val notNewlines_? = notNewline.rep
  val digit = CharIn('0' to '9')
  val AnInt = P( digit.rep(1) ).!.map(_.toInt)
  val ALong = P( digit.rep(1) ).!.map(_.toLong)
  val lowerCase = P( CharIn('a' to 'z') )
  val lowerCases = lowerCase.rep(1)
  val upperCase = P( CharIn('A' to 'Z') )
  val upperCases = upperCase.rep(1)
  val letter =  lowerCase | upperCase
  val letters = letter.rep(1)
  val dblQuot = "\""
  val sglQuot = "'"

  val blankLine = spaces_? ~ newline
  val DMY = P(DD ~ "-" ~ MMM ~ "-" ~ YYYY)
  val DD = P(digit ~ digit)
  val MMM = P(upperCase ~ upperCase ~ upperCase)
  val YYYY = P(digit ~ digit ~ digit ~ digit)

  // headers sometimes have two blank lines at the end, or one at the end and one at the beginning. Who knows why?
  val GenbankHeader = P(
    blankLine.? ~
    FileNameLine ~/
    DateLine ~/
    blankLine ~/
    ReleaseNumberLine ~/
    blankLine ~/
    TitleLine ~/
    blankLine ~/
    StatsLine ~/
    blankLine ~/
    blankLine.?) map { case (fileName, databaseName, releaseDate, releaseNumber, title, (entries, bases, reports)) =>
      genbank.GenbankHeader(fileName, databaseName, releaseDate, releaseNumber, title, entries, bases, reports) }

  
  val FileNameLine = P(notSpaces.! ~ spaces ~ notNewlines.! ~ newline)
  val DateLine = P(spaces ~ notNewlines.! ~ newline)
  val ReleaseNumberLine = P(spaces ~ notNewlines.! ~ newline)
  val TitleLine = P(spaces ~ notNewlines.! ~ newline)
  val StatsLine = P(
    spaces ~ AnInt ~
    spaces ~ "loci," ~
    spaces ~ ALong ~
    spaces ~ "bases, from" ~
    spaces ~ AnInt ~
    spaces ~ "reported sequences" ~
    spaces_? ~ newline)

  val stansaIndent = space.rep(min = 12, max = 12)
  val featureKeyIndent = space.rep(min = 5, max = 5)
  val featureQualifierIndent = space.rep(min = 21, max = 21)

  lazy val GenbankRecord = P(
    Locus ~
      Definition ~
      Accession ~
      Version ~
      DbLink.? ~
      Keywords ~
      Segment.? ~
      Source ~
      Reference.rep ~
      Comment.? ~
      Features ~
      SeqOrContig ~
      RecordSeparator) map genbank.GenbankRecord.tupled

  lazy val Locus = P(
    keyword("LOCUS") ~ notSpaces.! ~  // locusName
    spaces ~ AnInt ~ spaces ~ "bp" ~  // length
    spaces ~ notSpaces.! ~            // moleculeType
    spaces ~ notSpaces.! ~            // moleculeTopology
    spaces ~ upperCases.! ~           // division
    spaces ~ DMY.! ~                  // modificationDate
    newline
    ) map genbank.Locus.tupled
  
  lazy val Definition = P(
    keyword("DEFINITION") ~ multiline(notNewlines.!) ~ newline
  ) map { case(lines) => genbank.Definition(lines.mkString(sep="\n")) }
  
  lazy val Accession = P(
    keyword("ACCESSION") ~ multiline(AccessionList) ~ newline
  ) map { case(ass) => genbank.Accession(ass.flatten) }

  lazy val AccessionList = P( (!newline ~ !space ~ AnyChar).rep(1).!.rep(min = 1, sep = spaces) )

  lazy val Version = P(
    keyword("VERSION") ~ notSpaces.! ~ spaces ~ notNewlines.! ~ newline
  ) map genbank.Version.tupled

  lazy val DbLink = P(
    keyword("DBLINK") ~ multiline(Link) ~ newline
  ) map genbank.DbLink

  lazy val Link = P((!":" ~ AnyChar).rep(1).! ~ ":" ~ spaces ~ CrossRefIds) map genbank.Link.tupled

  lazy val CrossRefIds = P(
    (!space ~ !newline ~ !comma ~ AnyChar).rep(1).!.rep(min = 1, sep = comma ~ (spaces | (newline ~ stansaIndent)))
  )

  lazy val SemiColonPeriodList = P(SemiColonPeriodEntry.rep(sep=";" ~ spaces_?)) ~ period

  lazy val SemiColonPeriodEntry = P(
    (SemiColonPeriodSpan | SemiColonPeriodLineWrap).rep(1).map(_.mkString)
  )

  lazy val SemiColonPeriodSpan = P(
    (!newline ~ !";" ~ !(period ~ &(newline)) ~ AnyChar).rep(1).!
  )

  lazy val SemiColonPeriodLineWrap = P(
    period.!.? ~ newline ~ stansaIndent
  ) map (_ getOrElse "")

  lazy val Keywords = P(
    keyword("KEYWORDS") ~ SemiColonPeriodList ~ newline
  ) map genbank.Keywords

  lazy  val Segment = P(
    keyword("SEGMENT") ~ AnInt ~ spaces ~ "of" ~ spaces ~ AnInt ~ newline
  ) map genbank.Segment.tupled

  lazy  val Source = P(
    keyword("SOURCE") ~ multiline(notNewlines.!).map(_.mkString("\n")) ~ newline ~
    Organism
  ) map genbank.Source.tupled

  lazy val Organism = P(
    subkeyword("ORGANISM") ~ notNewlines.! ~ newline ~
    stansaIndent ~ SemiColonPeriodList ~ newline
  ) map genbank.Organism.tupled

  lazy  val Reference = P(
    ReferenceReference ~
    ReferenceAuthors.? ~
    ReferenceConsortium.rep ~
    ReferenceTitle.? ~
    ReferenceJournal ~
    ReferenceMedline.? ~
    ReferencePubmed.? ~
    ReferenceRemark.?
  ) map genbank.Reference.tupled

  lazy val ReferenceReference = P(
    keyword("REFERENCE") ~ AnInt ~ ReferenceCoverage ~ newline
  )

  lazy val lsep = P(
    spaces | lineWrap
  )

  lazy val lineWrap = P(newline ~ stansaIndent)

  lazy val ReferenceCoverage = P( (spaces ~ "(" ~ (Bases | Sites) ~ ")").? ) map (_ getOrElse Seq.empty)

  lazy val Bases = P(
    "bases" ~/ lsep ~ BaseRange.rep(min = 1, sep = ";" ~/ lsep)
  )

  lazy val Sites = P(
    "sites".~/ map (_ => Seq.empty[BaseRange])
  )

  lazy val BaseRange = P(
    AnInt ~ lsep ~ "to" ~/ lsep ~ AnInt
  ) map genbank.BaseRange.tupled

  lazy  val ReferenceAuthors = P(
    subkeyword("AUTHORS") ~ AuthorList ~ period.? ~ newline
  )

  lazy val AuthorList = P(
    FinalAuthor.map(a => genbank.AuthorList(Seq(a), false)) |
      (AuthorListL ~ (AndAuthor | EtAl)).map { case (al, f) => f(al) }
  ) log "AuthorList"

  lazy val AuthorListL: P[genbank.AuthorList] = P(
    Author.rep(min = 1, sep = ("," ~ ",".? ~ lsep).~/ | lineWrap)
  ) map (genbank.AuthorList(_, false)) log "AuthorListL"

  lazy val FinalAuthor = P(Author ~ period ~ newline ~ !stansaIndent)

  lazy val Author: P[genbank.Author] = P(
    WellFormedAuthor |
      NameWithTrailingSpace |
      NameWithNamesInInitials |
      InitialsThenName |
      FamilyNameOnly
  ) log "Author"

  lazy val WellFormedAuthor = P(
    FamilyName ~ (comma | (period ~ comma.?) | (newline ~ stansaIndent) ) ~ Initials ~ (lsep ~ NameNumber).? ~ &(thenNameEnding)
  ) map { case (fn, is, nn) => genbank.Author(Some(fn), is, nn) } log "WellFormedAuthor"

  lazy val NameWithTrailingSpace = P(
    FamilyName ~ (comma | period) ~ space ~ Initials ~ (lsep ~ NameNumber).? ~ &(thenNameEnding)
  ) map { case (fn, is, nn) => genbank.Author(Some(fn), is, nn) } log "NameWithTrailingSpace"

  lazy val NameWithNamesInInitials = P(
    FamilyName ~ comma ~ InitialsWithNamesIn ~ (lsep ~ NameNumber).? ~ &(thenNameEnding)
  ) map { case (fn, is, nn) => genbank.Author(Some(fn), is, nn) } log "NameWithNamesInInitials"

  lazy val FamilyNameOnly = P(
    FamilyName ~ (&(thenNameEnding) | &(lineWrap))
  ) map (fn => genbank.Author(Some(fn), Seq(), None)  ) log "FamilyNameOnly"

  lazy val InitialsThenName = P(
    Initials ~ lsep ~ FamilyName ~ &(thenNameEnding)
  ) map { case (is, fn) => genbank.Author(Some(fn), is, None) } log "InitialsThenName"

  lazy val FamilyName = P(
    CompoundName |
    SimpleName
  ) log "FamilyName"

  lazy val SimpleName = P( ((letter log "firstLetter") ~ (letter.log("l") | hyphen.log("h") | sglQuot.log("sglQ")).log("insideLetter").rep(1, sep = "".log("insideLetterSep"))).!.log("letters") ) log "SimpleName"

  lazy val CompoundName = P(
    ((upperCase ~ lowerCase.rep(1) ~ period).! | (letter ~ lowerCase.rep).!) ~ lsep ~ SimpleName
  ) map { case (pfx, sfx) => s"$pfx $sfx" } log "CompoundName"

  lazy val Initials = P( (Initial ~ (&(thenEndingComma) | (period.? ~ comma ~ &(upperCase)) | period)).rep(1) ) log "Initials"

  lazy val Initial = P((hyphen.? ~ upperCase).!) log "Initial"

  lazy val InitialsWithNamesIn = P(
    Initials ~ FamilyName ~ period ~ Initials
  ) map { case (i1, fn, i2) => (i1 :+ fn) ++ i2 } log "InitialsWithNamesIn"

  lazy val NameNumber = P( (upperCase ~ letter.rep(1) ~ period.?).! ~ &(thenNameEnding)) log "NameNumber"

  lazy val listAnd = P( lsep ~ "and") log "listAnd"

  lazy val listEt = P( lsep ~ "et") log "listEt"

  lazy val AndAuthor = P( lsep ~ "and" ~/ lsep ~ Author) map (postpendAuthor _) log "AndAuthor"

  lazy val EtAl = P( lsep ~ "et" ~ lsep ~ "al." ) map (_ => withEtAl _) log "EtAl"

  private def prependAuthor(au: genbank.Author) = (al: genbank.AuthorList) => al.copy(authors = au +: al.authors)
  private def postpendAuthor(au: genbank.Author) = (al: genbank.AuthorList) => al.copy(authors = al.authors :+ au)
  private def withEtAl(al: genbank.AuthorList) = al.copy(etAl = true)

  lazy val thenEndingComma = P(comma ~ !letter)

  lazy val thenNameEnding = P(thenEndingComma | listAnd | listEt | (period.? ~ newline ~ !stansaIndent))

  lazy val ReferenceConsortium = P(
    subkeyword("CONSRTM") ~ multiline(notNewlines).! ~ newline
  ) map genbank.Consortium

  lazy val ReferenceTitle = P(
    subkeyword("TITLE") ~ multiline(notNewlines.!) ~ newline
  ) map (_.mkString("\n"))

  lazy  val ReferenceJournal = P(
    subkeyword("JOURNAL") ~ multiline(notNewlines.!) ~ newline
  ) map (_.mkString("\n"))

  lazy  val ReferenceMedline = P(
    subkeyword("MEDLINE") ~ AnInt ~ newline
  )

  lazy  val ReferencePubmed = P(
    subkeyword(" PUBMED") ~ AnInt ~ newline
  )

  lazy  val ReferenceRemark = P(
    subkeyword("REMARK") ~ multiline(notNewlines.!) ~ newline
  ) map (_.mkString("\n"))

  lazy  val Comment = P(
    keyword("COMMENT") ~ multilineC(notNewlines_?.!) ~ newline
  ) map genbank.Comment

  lazy  val Features = P(
    padTo(21, "FEATURES") ~ "Location/Qualifiers" ~ newline ~
    Feature.rep(1)
  )

  lazy  val Feature = P(
    indentAndPad(5, 21, notSpaces.!.~/) ~ Location ~ newline ~
    Qualifier.rep
  ) map genbank.Feature.tupled

  lazy val Location = P( Position | Operation | RemoteReference )

  lazy val Position = P( Extent | Point )

  lazy val Extent = P( Span | Cut | OneOf )

  lazy val Point = P( AtOrAfter | AtOrBefore | SingleBase | NestedOneOf )

  lazy val Operation = P( Complement | Join | Order )

  lazy val RemoteReference = P(
    ( upperCase ~ (upperCase | digit | period).rep).! ~ ":" ~/ Position
  ) map genbank.Location.RemoteReference.tupled

  lazy val Span = P(
    Point ~ ".." ~ Point
  ) map genbank.Location.Span.tupled

  lazy val OneOf = P(
    SingleBase ~ period ~ SingleBase
  ) map genbank.Location.OneOf.tupled

  lazy val Cut = P(
    SingleBase ~ "^" ~ SingleBase
  ) map genbank.Location.Cut.tupled

  lazy val AtOrAfter = P(
    ">" ~ SingleBase
  ) map genbank.Location.AtOrAfter

  lazy val AtOrBefore = P(
    "<" ~ SingleBase
  ) map genbank.Location.AtOrBefore

  lazy val NestedOneOf = P(
    "(" ~ OneOf ~ ")"
  ) map genbank.Location.NestedOneOf

  lazy val SingleBase = P( AnInt ) map genbank.Location.SingleBase

  lazy val Complement: Parser[genbank.Location.Complement] = P(
    "complement(" ~/ Location ~ ")"
  ) map genbank.Location.Complement

  lazy val Join: Parser[genbank.Location.Join] = P(
    "join(" ~/ Location.rep(min=1, sep=comma ~ (newline ~ featureQualifierIndent).?) ~ ")"
  ) map genbank.Location.Join

  lazy val Order: Parser[genbank.Location.Order] = P(
    "order(" ~/ Location.rep(min=1, sep=comma ~ (newline ~ featureQualifierIndent).?) ~ ")"
  ) map genbank.Location.Order

  lazy val Qualifier = P(
    featureQualifierIndent ~ "/" ~/ QualifierName ~ ("=" ~/ QualifierValue).? ~ newline
  ) map genbank.Qualifier.tupled

  lazy val QualifierName = P((!"=" ~ !newline ~ !space ~ AnyChar).rep(1).!)

  lazy val QualifierValue = QuotedValue | UnquotedValue

  lazy val QuotedValue = P(
    dblQuot ~/ multilineQ((!newline ~ ((dblQuot ~ !newline) | (!dblQuot ~ AnyChar))).rep.!) ~ dblQuot
  ) map (txts => genbank.QuotedValue(txts.mkString("\n")))

  lazy val UnquotedValue = P(
    notNewlines.! ~ (newline ~ featureQualifierIndent ~ !("/" ~ !space) ~ notNewlines.!).rep
  ) map { case (fstLine, lines) => genbank.UnquotedValue((fstLine +: lines).mkString("\n")) }

  lazy val SeqOrContig = P(
    (Origin ~ Sequence).map(Left.apply) | Contig.map(Right.apply)
  )

  lazy val Origin = P(
    keyword("ORIGIN") ~ notNewline.rep(1).!.? ~ newline
  ) map genbank.Origin

  lazy val Sequence = SequenceLine.rep map (seq => genbank.Sequence(seq.mkString))

  lazy val SequenceLine = P(
    spaces ~ AnInt ~ space ~ notNewlines.! ~ newline
  ) map { case (_, seq) => seq.replaceAll(" ", "") }

  lazy val RecordSeparator = P(
    "//" ~ newline
  )

  lazy val Contig = P(
    keyword("CONTIG") ~ "join(" ~/ ContigParts ~ ")" ~ newline
  ) map genbank.Contig

  lazy val ContigParts = P(
    ContigPart.rep(min = 1, sep = comma ~/ (lineWrap).?)
  )

  lazy val ContigPart = P(
    ContigGap | ContigComplement | ContigRemoteReference | ContigSpan
  )

  lazy val ContigGap = P(
    KnownGap | UnknownNs | UnknownGap
  )

  lazy val KnownGap = P(
    "gap(" ~ AnInt ~ ")"
  ) map genbank.ContigPart.KnownGap

  lazy val UnknownNs = P(
    "gap(unk" ~ AnInt ~ ")"
  ) map genbank.ContigPart.UnknownNs

  lazy val UnknownGap = P(
    "gap()"
  ) map (_ => genbank.ContigPart.UnknownGap)

  lazy val ContigComplement = P(
    "complement(" ~/ ContigRemoteReference ~ ")"
  ) map genbank.ContigPart.Complement

  lazy val ContigRemoteReference = P(
    ( upperCase ~ (upperCase | digit | period).rep).! ~ ":" ~/ ContigSpan
  ) map genbank.ContigPart.RemoteReference.tupled

  lazy val ContigSpan = P(
    ContigSingleBase ~ ".." ~ ContigSingleBase
  ) map genbank.ContigPart.Span.tupled

  lazy val ContigSingleBase = P(
    AnInt
  ) map genbank.ContigPart.SingleBase

  lazy val GenbankRecords = P(GenbankRecord.rep)

  /** A genbank database file */
  lazy val GenbankDatabaseFile = P(GenbankHeader ~ GenbankRecords ~ End)

  /** A file containing mulitple Genbank records */
  lazy val GenbankFile = P(GenbankRecords ~ End)


  def keyword(kw: String) = padTo(12, kw)

  def subkeyword(kw: String) = indentAndPad(2, 12, kw)

  def indentAndPad(indent: Int, length: Int, kw: String) = {
    val l = length - kw.length - indent
    space.rep(min = indent, max = indent) ~ kw ~/ space.rep(min = l, max = l)
  }

  def indentAndPad(indent: Int, length: Int, kw: Parser[String]) = {
    (space.rep(min = indent, max = indent) ~ kw.~/) flatMap { case m =>
      val l = length - indent - m.length

      space.rep(min = l, max = l) map (_ => m)
    }
  }

  def padTo(length: Int, kw: String) = {
    val l = length - kw.length
    kw ~/ space.rep(min=l, max=l)
  }

  def multiline[T](p: Parser[T]): Parser[Seq[T]] =
    ((p ~ lineWrap).rep ~ p) map { case (qs, q) => qs :+ q }

  def multilineC[T](p: Parser[T]): Parser[Seq[T]] =
    ((p ~ newline ~ (stansaIndent | &(newline))).rep ~ p) map { case (qs, q) => qs :+ q }

  def multiline0[T](p: Parser[T]): Parser[Seq[T]] =
    multiline(p) | Pass.map(_ => Seq())

  def multilineQ[T](p: Parser[T]): Parser[Seq[T]] =
    ((p ~ newline ~ featureQualifierIndent).rep ~ p) map { case (qs, q) => qs :+ q }
}
