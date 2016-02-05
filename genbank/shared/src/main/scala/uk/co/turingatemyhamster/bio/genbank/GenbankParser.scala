package uk.co.turingatemyhamster.bio
package genbank

import fastparse.all._



/**
 * See: ftp://ftp.ncbi.nih.gov/genbank/gbrel.txt
 * See: http://www.insdc.org/files/feature_table.html
 *
 * @author Matthew Pocock
 */
object GenbankParser {

  val newline = "\r\n" | "\r" | "\n"
  val space = " "
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
  val dblDblQuot = "\"\""

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
    (!space ~ !newline ~ !"," ~ AnyChar).rep(1).!.rep(min = 1, sep = "," ~ (spaces | (newline ~ stansaIndent)))
  )

  lazy val SemiColonPeriodList = P(SemiColonPeriodEntry.rep(sep=";" ~ spaces_?)) ~ "."

  lazy val SemiColonPeriodEntry = P(
    (SemiColonPeriodSpan | SemiColonPeriodLineWrap).rep(1).map(_.mkString)
  )

  lazy val SemiColonPeriodSpan = P(
    (!newline ~ !";" ~ !("." ~ &(newline)) ~ AnyChar).rep(1).!
  )

  lazy val SemiColonPeriodLineWrap = P(
    ".".!.? ~ newline ~ stansaIndent
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

  lazy val spacesOrLineWrap = P(
    (spaces | lineWrap).rep(1)
  )

  lazy val lineWrap = P(newline ~ stansaIndent)

  lazy val ReferenceCoverage = P( (spaces ~ "(" ~ (Bases | Sites) ~ ")").? ) map (_ getOrElse Seq.empty)

  lazy val Bases = P(
    "bases" ~/ spacesOrLineWrap ~ BaseRange.rep(min = 1, sep = ";" ~/ spacesOrLineWrap)
  )

  lazy val Sites = P(
    "sites".~/ map (_ => Seq.empty[BaseRange])
  )

  lazy val BaseRange = P(
    AnInt ~ spacesOrLineWrap ~ "to" ~/ spacesOrLineWrap ~ AnInt
  ) map genbank.BaseRange.tupled

  lazy  val ReferenceAuthors = P(
    subkeyword("AUTHORS") ~ AuthorList ~ ".".? ~ newline
  )

  lazy val AuthorList = P(
    Author.rep(min = 1, sep = (",".rep(1) ~ spacesOrLineWrap) | (spacesOrLineWrap ~ "and" ~/ spacesOrLineWrap))
  )

  lazy val Author = P(
    Author_WellFormed |
       Author_BrokenInitial |
       Author_BrokenNameInitials |
       Author_OnlyFirstname |
       Author_InitialsThenName |
       Author_OnlyInitials
  ) map genbank.Author.tupled

  lazy val Author_WellFormed = P(
    FirstName.map(Some.apply) ~ ((".".? ~ ",") | lineWrap) ~ Initials ~ (spacesOrLineWrap ~ NameNumber).?
  )

  lazy val Author_InitialsThenName = P(
    Initials ~ spaces_? ~ FirstName ~ &(("," ~ (spaces | newline)) | " and" | ("." ~ newline ~ !stansaIndent))
  ) map { case (is, fn) => (Some(fn), is, None) }

  lazy val Author_BrokenInitial = P(
    FirstName ~ "," ~ (!"," ~ !newline ~ !"." ~ !" " ~ AnyChar).! ~ &(",")
  ).map { case (f, i) => (Some(f), Seq(i), None) }

  lazy val Author_OnlyFirstname = P(
    FirstName.map(n => (Some(n), Seq(), None)) ~ (&(",") | &("." ~ newline) | &(spacesOrLineWrap ~ "and" ~/ spacesOrLineWrap))
  )

  lazy val Author_OnlyInitials = P(
    Initials map ((None, _, None))
  )

  lazy val Author_BrokenNameInitials = P(
    BrokenFirstName ~ ". " ~ Initials
  ) map { case (f, i) => (Some(f), i, None) }

  lazy val FirstName = P(
    nameChar.rep(1).! ~ (lineWrap.map(_ => " ") ~ nameChar.rep(1).! ~ &("," | " and")).?
  ) map { case (n1, n2) => n2.fold(n1) { case(n, nr) => n1 + n + nr } }

  lazy val nameChar = P(!"," ~ (". " | (!newline ~ !"." ~ !" and" ~ AnyChar)))

  lazy val BrokenFirstName = P(
    (!"," ~ (!newline ~ !"." ~ !" and" ~ AnyChar)).rep(1).!
  )

  lazy val Initials = P((Initial ~ ".").rep(1))

  lazy val Initial = P(
    (lineWrap ~ !"and " ~ !NameNumber).? ~ !"," ~ !" " ~ (!newline ~ !"." ~ !" and" ~ AnyChar ~ !",").rep(1).!
  )

  lazy val NameNumber = P(
    !"and" ~ (!"," ~ !" " ~ !newline ~ AnyChar).rep(1).!
  )

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
    ( upperCase ~ (upperCase | digit | ".").rep).! ~ ":" ~/ Position
  ) map genbank.Location.RemoteReference.tupled

  lazy val Span = P(
    Point ~ ".." ~ Point
  ) map genbank.Location.Span.tupled

  lazy val OneOf = P(
    SingleBase ~ "." ~ SingleBase
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
    "join(" ~/ Location.rep(min=1, sep="," ~ (newline ~ featureQualifierIndent).?) ~ ")"
  ) map genbank.Location.Join

  lazy val Order: Parser[genbank.Location.Order] = P(
    "order(" ~/ Location.rep(min=1, sep="," ~ (newline ~ featureQualifierIndent).?) ~ ")"
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
    ContigPart.rep(min = 1, sep = "," ~/ (lineWrap).?)
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
    ( upperCase ~ (upperCase | digit | ".").rep).! ~ ":" ~/ ContigSpan
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
