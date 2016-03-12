package uk.co.turingatemyhamster.bio.genbank

import utest._
import fastparse.all._
import fastparse.core.Parsed.Success
import fastparse.core.Parsed.Failure

/**
  *
  *
  * @author Matthew Pocock
  */
object GenbankParserTestSuite extends TestSuite {

  val tests = TestSuite {

    'locations {

      'SingleBase {
        * - {
          val loc = "1"

          'SingleBase - GenbankParser.SingleBase.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

        * - {
          val loc = "1234"

          'SingleBase - GenbankParser.SingleBase.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }
      }

      'AtOrBefore {
        * - {
          val loc = "<1"

          'AtOrBefore - GenbankParser.AtOrBefore.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

        * - {
          val loc = "<1234"

          'AtOrBefore - GenbankParser.AtOrBefore.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

      }

      'AtOrAfter {
        * - {
          val loc = ">1"

          'AtOrAfter - GenbankParser.AtOrAfter.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

        * - {
          val loc = ">1234"

          'AtOrAfter - GenbankParser.AtOrAfter.parse(loc).get
          'Point - GenbankParser.Point.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

      }

      'Span {
        * - {
          val loc = "<1..291"

          'Span - GenbankParser.Span.parse(loc).get
          'Extent - GenbankParser.Extent.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

        * - {
          val loc = "1..>284"

          'Span - GenbankParser.Span.parse(loc).get
          'Extent - GenbankParser.Extent.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }
      }

      'Order {
        * - {
          val loc = "order(<1..291,1..>284)"

          'Order - GenbankParser.Order.parse(loc).get
          'Operation - GenbankParser.Operation.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }
      }

      'Remote {
        * - {
          val loc = "AF003391.1:<1..291"

          'RemoteReference - GenbankParser.RemoteReference.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }
      }

      'pathological {

        * - parse(GenbankParser.Location, "order(AF003391.1:<1..291,1..>284)")
        * - parse(GenbankParser.Location, "order(S66019.1:1,S66020.1:1,1)")

      }

      'nested {
        * - {
          val loc = "(1285.1287)"

          GenbankParser.NestedOneOf.parse(loc).get
        }

        * - {
          val loc = "(1285.1287)..(1285.1287)"

          'Span - GenbankParser.Span.parse(loc).get
          'Extent - GenbankParser.Extent.parse(loc).get
          'Position - GenbankParser.Position.parse(loc).get
          'Location - GenbankParser.Location.parse(loc).get
        }

      }
    }

    'qualifiers {
      'unquoted {
        * - {
          val v = parse(GenbankParser.UnquotedValue,
                      """1
                        |                     /db_xref="GI:583836715"
                        |""".stripMargin)
          assert(v == UnquotedValue("1"))
        }
      }

      'names {
        * - GenbankParser.QualifierName.parse("locus_tag").get
        * - GenbankParser.QualifierName.parse("pseudo").get
        * - GenbankParser.QualifierName.parse("note").get
      }

      'single {
        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /locus_tag="NTE_00096"
              |                     /note="IMG reference gene:2527215782;IMG reference
              |                     gene:2527215783;uracil-DNA glycosylase"
              |     gene            75644..76000
              |""".stripMargin)
          assert(v == Qualifier("locus_tag", Some(QuotedValue("NTE_00096"))))
        }

        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /note="IMG reference gene:2527215782;IMG reference
              |                     gene:2527215783;uracil-DNA glycosylase"
              |                     /pseudo
              |     gene            75644..76000
              |""".stripMargin)
          assert(v == Qualifier("note", Some(QuotedValue(
            "IMG reference gene:2527215782;IMG reference\ngene:2527215783;uracil-DNA glycosylase"))))
        }

        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /pseudo
              |     gene            75644..76000
              |""".stripMargin)
          assert(v ==
            Qualifier("pseudo", None))
        }

        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /locus_tag="AX25_00010"
              |                     /inference="EXISTENCE: similar to AA
              |                     sequence:RefSeq:YP_004856441.1"
              |""".stripMargin)
            assert(v ==
              Qualifier("locus_tag", Some(QuotedValue("AX25_00010"))))
        }

        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /inference="EXISTENCE: similar to AA
              |                     sequence:RefSeq:YP_004856441.1"
              |                     /codon_start=1
              |""".stripMargin)
          assert(v == Qualifier("inference", Some(QuotedValue(
            "EXISTENCE: similar to AA\nsequence:RefSeq:YP_004856441.1"))))
        }

        * - {
          val v = parse(GenbankParser.Qualifier,
            """                     /codon_start=1
              |                     /db_xref="GI:583836715"
              |""".stripMargin)
          assert(v == Qualifier("codon_start", Some(UnquotedValue("1"))))
        }
      }

      'multiple {
        * - {
          val v = parse(GenbankParser.Qualifier.rep, """                     /locus_tag="NTE_00096"
                                                      |                     /note="IMG reference gene:2527215782;IMG reference
                                                      |                     gene:2527215783;uracil-DNA glycosylase"
                                                      |                     /pseudo
                                                      |     gene            75644..76000
                                                      |""".stripMargin)
          assert(v.size == 3)
        }

        * - {
          val v = parse(GenbankParser.Qualifier.rep ~ End,
            """                     /locus_tag="NTE_00096"
              |                     /note="IMG reference gene:2527215782;IMG reference
              |                     gene:2527215783;uracil-DNA glycosylase"
              |                     /pseudo
              |""".stripMargin)
          assert(v.size == 3)
        }

        * - parse(GenbankParser.Qualifier, """                     /note="IMG reference gene:2527215782;IMG reference
                                            |                     gene:2527215783;uracil-DNA glycosylase"
                                            |                     /pseudo
                                            |     gene            75644..76000
                                            |""".stripMargin)
        * - assert(parse(GenbankParser.Qualifier, """                     /pseudo
                                            |     gene            75644..76000
                                            |""".stripMargin) == Qualifier("pseudo", None))
      }
    }

    'feature {
      * - {
        val f = parse(GenbankParser.Feature, """     gene            complement(join(2919018..2919538,1..199))
                                              |                     /locus_tag="AX25_00005"
                                              |                     /pseudo
                                              |     gene            complement(387..1190)
                                              |""".stripMargin)

        assert(f.key == "gene")
        assert(f.annotations.size == 2)
      }

      * - {
        val f = parse(GenbankParser.Feature, """     CDS             complement(387..1190)
                                              |                     /locus_tag="AX25_00010"
                                              |                     /inference="EXISTENCE: similar to AA
                                              |                     sequence:RefSeq:YP_004856441.1"
                                              |                     /codon_start=1
                                              |                     /db_xref="GI:583836715"
                                              |     gene            complement(1204..2196)
                                              |""".stripMargin)

        assert(f.key == "CDS")
        assert(f.annotations.size == 4)
      }

      * - {
        val txt = """     CDS             complement(241476..242309)
                    |                     /locus_tag="Thal_0247"
                    |                     /inference="protein motif:TFAM:TIGR00097"
                    |                     /note="KEGG: sat:SYN_01521 phosphomethylpyrimidine kinase
                    |                     / hydroxymethylpyrimidine kinase;
                    |                     TIGRFAM: phosphomethylpyrimidine kinase;
                    |                     PFAM: Phosphomethylpyrimidine kinase type-1"
                    |                     /codon_start=1
                    |     gene            complement(242309..244633)""".stripMargin

        val f = parse(GenbankParser.Feature, txt)

        assert(f.key == "CDS")
        assert(f.annotations.size == 4)

        f
      }

      * - {
        val txt = """     CDS             559308..560831
                    |                     /transl_table=11
                    |                     /product="vinylacetyl-CoA delta-isomerase
                    |                     /4-hydroxybutyryl-CoA dehydratase"
                    |                     /protein_id="AIF82725.1"
                    |                     /db_xref="GI:665992965"
                    |     gene            560898..561266
                    |""".stripMargin

        val f = parse(GenbankParser.Feature, txt)

        assert(f.key == "CDS")
        assert(f.annotations.size == 4)

        f
      }

      * - {
        val txt = """     CDS             5956125..5957648
                    |                     /codon_start=1
                    |                     /transl_table=11
                    |                     /product="IMP cyclohydrolase
                    |                     /phosphoribosylaminoimidazolecarboxamide
                    |                     formyltransferase"
                    |                     /protein_id="CBK69708.1"
                    |     gene            5957744..5958766
                    |""".stripMargin

        val f = parse(GenbankParser.Feature, txt)

        assert(f.key == "CDS")
        assert(f.annotations.size == 4)

        f
      }

      * - {
        val txt = """     CDS             complement(701541..702758)
                    |                     /locus_tag="OOM_0715"
                    |                     /note="COG0814;Trp_Tyr_perm;(HMMPfam:IPR018227);Tryptophan
                    |                     /tyrosine
                    |                     permease;AROAAPRMEASE;(FPrintScan:IPR002091);Aromatic
                    |                     amino acid permease;Biological Process: amino acid
                    |                     transport (GO:0006865), Cellular Component: membrane
                    |                     (GO:0016020)"
                    |                     /codon_start=1
                    |                     /transl_table=11
                    |     gene            complement(702772..703221)
                    |""".stripMargin

        val f = parse(GenbankParser.Feature, txt)

        assert(f.key == "CDS")
        assert(f.annotations.size == 4)

        f
      }

      'brokenQualifier - {
        val txt = """     source          1..1300
                    |                     /organism="Candidatus Stammerula sp. of Trupanea
                    |                     "pohakuloa""
                    |                     /mol_type="genomic DNA"
                    |                     /isolate="Tpohsym"
                    |                     /isolation_source="extraperitrophic area of midgut"
                    |                     /host="Trupanea sp. 'pohakuloa' ex Dubautia linearis"
                    |                     /db_xref="taxon:1630665"
                    |                     /country="USA: Archipelago Hawaii, Hawaii, Mauna Kea SP"
                    |     rRNA            1..1300
                    |""".stripMargin

        parse(GenbankParser.Feature, txt)
      }
    }

    'dblink {
      * - {
        val txt = """DBLINK      BioProject: PRJNA202883
                    |            BioSample: SAMN02713684
                    |            Sequence Read Archive: SRZ080794
                    |KEYWORDS    .
                    |""".stripMargin

        val l = parse(GenbankParser.DbLink, txt)
        assert(l.links.size == 3)
        assert(l.links.head.crossRefIds.head == "PRJNA202883")
      }

      * - {
        val txt = """DBLINK      BioProject: PRJNA202380
                    |            Sequence Read Archive: SRR873595, SRR873596, SRR873597, SRR873598,
                    |            SRR873599, SRR873600, SRR873601, SRR873602, SRR873603, SRR873604,
                    |            SRR873605, SRR873606, SRR873607, SRR873608, SRR873609, SRR873610
                    |KEYWORDS    ENV.
                    |""".stripMargin
        val l = parse(GenbankParser.DbLink, txt)
        assert(l.links.size == 2)
        assert(l.links.tail.head.crossRefIds.size == 16)
        assert(l.links.tail.head.crossRefIds.head == "SRR873595")
      }
    }

    'source {
      * - {
        val txt = """SOURCE      Exiguobacterium sp. N34
                    |  ORGANISM  Exiguobacterium sp. N34
                    |            Bacteria; Firmicutes; Bacilli; Bacillales; Bacillales Family XII.
                    |            Incertae Sedis; Exiguobacterium.
                    |REFERENCE   1
                    |""".stripMargin

        val s = parse(GenbankParser.Source, txt)
        assert(s.value == "Exiguobacterium sp. N34")
        assert(s.organism.name == "Exiguobacterium sp. N34")
        assert(s.organism.values.size == 6)
      }

      * - {
        val txt = """SOURCE      Bacillus sp. N36
                    |  ORGANISM  Bacillus sp. N36
                    |            Bacteria; Firmicutes; Bacilli; Bacillales; Bacillaceae; Bacillus.
                    |REFERENCE   1
                    |""".stripMargin

        val s = parse(GenbankParser.Source, txt)
        assert(s.value == "Bacillus sp. N36")
        assert(s.organism.name == "Bacillus sp. N36")
        assert(s.organism.values.size == 6)
      }
    }

    'keywords {
      * - {
        val kw = """KEYWORDS    .
                           |""".stripMargin

        val kws = parse(GenbankParser.Keywords, kw)

        assert(kws.values.size == 0)
      }

      * - {
        val kw = """KEYWORDS    16S ribosomal RNA; 16S rRNA gene; internal transcribed spacer 1;
                   |            ITS1.
                   |""".stripMargin

        val kws = parse(GenbankParser.Keywords, kw)

        assert(kws.values.size == 4)
      }

      * - {
        val kw = """KEYWORDS    hydrogen-uptake [NiFe] hydrogenase large subunit; hyn operon; hynL
                   |            gene.
                   |""".stripMargin

        val kws = parse(GenbankParser.Keywords, kw)

        assert(kws.values.size == 3)
      }
    }

    'initials {
      * - {
        val txt = "J.J. III,"
        val is = parse(GenbankParser.Initial, txt)
        assert(is == "J")
      }

      * - {
        val txt = "J. III,"
        val is = parse(GenbankParser.Initial, txt)
        assert(is == "J")
      }

      * - {
        val txt = " III,"
        noParse(GenbankParser.Initial ~ End, txt)
      }

      * - {
        val txt = "III,"
        val nn = parse(GenbankParser.NameNumber, txt)
        assert(nn == "III")
      }

      * - {
        val txt = "J.J. III,"
        val is = parse(GenbankParser.Initials, txt)
        assert(is == Seq("J", "J"))
      }

      * - {
        val txt = """J.E.
                    |            III and Cookson, B.T.""".stripMargin
        val auth = parse(GenbankParser.Initials, txt)
        assert(auth == Seq("J", "E"))
      }

      * - {
        val txt = "D,A.,"
        val auth = parse(GenbankParser.Initials, txt)
        assert(auth == Seq("D", "A"))
      }

      * - {
        val txt = "J.-L."
        val auth = parse(GenbankParser.Initials, txt)
        assert(auth == Seq("J", "-L"))
      }
    }

    'author {

      * - {
        val txt = "R.B.Aronson,"
        val auth = parse(GenbankParser.InitialsThenName, txt)
        assert(auth.name.get == "Aronson")
        assert(auth.initials == Seq("R", "B"))
      }

      * - {
        val txt = "R.B.Aronson,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth.name.get == "Aronson")
        assert(auth.initials == Seq("R", "B"))
      }

      * - {
        val txt = "Duan,J.B.,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth.name.get == "Duan")
        assert(auth.initials == Seq("J", "B"))
      }

      * - {
        val txt = "Duan,J.B., Zou,M.J."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth.name.get == "Duan")
        assert(auth.initials == Seq("J", "B"))
      }

      * - {
        val txt = "Mathur,P. and St. Leger,R.J."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Mathur"), Seq("P"), None, None))
      }

      * - {
        val txt = "Michelangelo,S.Moerland.M.M., "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Michelangelo"), Seq("S", "Moerland", "M", "M"), None, None))
      }

      * - {
        val txt = "Vu.J., "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Vu"), Seq("J"), None, None))
      }

      * - {
        val txt = "Shveta, "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Shveta"), Seq(), None, None))
      }

      * - {
        val txt = "Shveta, Singh,D.P."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Shveta"), Seq(), None, None))
      }

      * - {
        val txt = "Kupko,J.J. III,\n            "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Kupko"), Seq("J", "J"), Some("III"), None))
      }

      * - {
        val txt = "Sheffield,V, Weber"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Sheffield"), Seq("V"), None, None))
      }

      * - {
        val txt = "da Silva,W. Jr.,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("da Silva"), Seq("W"), Some("Jr."), None))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson, B.T.""".stripMargin
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Clarridge"), Seq("J", "E"), Some("III"), None))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Arthi"), Seq("S"), None, None))
      }

      * - {
        val txt = "Shobana and Ananthi."
        val auth = parse(GenbankParser.SimpleName, txt)
        assert(auth == "Shobana")
      }

      * - {
        val txt = "Shobana and Ananthi."
        noParse(GenbankParser.CompoundName, txt)
      }

      * - {
        val txt = "Shobana and Ananthi."
        val auth = parse(GenbankParser.FamilyName, txt)
        assert(auth == "Shobana")
      }

      * - {
        val txt = "Ananthi."
        val auth = parse(GenbankParser.FamilyName, txt)
        assert(auth == "Ananthi")
      }

      * - {
        val txt = "Shobana and Ananthi."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Shobana"), Seq(), None, None))
      }

      * - {
        val txt = "Sukenik. A.,"
        val auth = parse(GenbankParser.NameWithTrailingSpace, txt)
        assert(auth == Author(Some("Sukenik"), Seq("A"), None, None))
      }

      * - {
        val txt = "Sukenik. A.,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Sukenik"), Seq("A"), None, None))
      }

      * - {
        val txt = "S.L. Miller.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Miller"), Seq("S", "L"), None, None))
      }

      * - {
        val txt = "Hoiby.,N.\n"
        val auth = parse(GenbankParser.WellFormedAuthor, txt)
        assert(auth == Author(Some("Hoiby"), Seq("N"), None, None))
      }

      * - {
        val txt = "Hoiby.,N.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Hoiby"), Seq("N"), None, None))
      }

      * - {
        val txt = "Schell,M.,A.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Schell"), Seq("M", "A"), None, None))
      }

      * - {
        val txt = "Siegele,D,A.,\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Siegele"), Seq("D", "A"), None, None))
      }

      * - {
        val txt = "Singh,J.A,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Singh"), Seq("J", "A"), None, None))
      }

      * - {
        val txt = "St. Leger,R.J.\n"
        val auth = parse(GenbankParser.FamilyName, txt)
        assert(auth == "St. Leger")
      }

      * - {
        val txt = "St. Leger,R.J.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("St. Leger"), Seq("R", "J"), None, None))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Clarridge"), Seq("J", "E"), Some("III"), None))
      }

      * - {
        val txt = "DAS GUPTA,T."
        val auth = parse(GenbankParser.upperCases.! ~ !GenbankParser.thenNameEnding, txt)
        assert(auth == "DAS")
      }

      * - {
        val txt = "DAS GUPTA,T."
        val auth = parse(GenbankParser.CompoundNamePrefix, txt)
        assert(auth == "DAS")
      }

      * - {
        val txt = "DAS GUPTA,T."
        val auth = parse(GenbankParser.CompoundName, txt)
        assert(auth == "DAS GUPTA")
      }

      * - {
        val txt = "DAS GUPTA,T.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("DAS GUPTA"), Seq("T"), None, None))
      }

    }

    'authorList {

      * - {
        val txt = "Duan,J.B., Zou,M.J.,"
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths.authors.size == 2)
        assert(auths.authors.head.name.get == "Duan")
        assert(auths.authors.head.initials == Seq("J", "B"))
        assert(auths.authors.tail.head.name.get == "Zou")
        assert(auths.authors.tail.head.initials == Seq("M", "J"))
      }

      * - {
        val txt = "Screen,S.E., Mathur,P. and St. Leger,R.J.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Screen"), Seq("S", "E"), None, None),
          Author(Some("Mathur"), Seq("P"), None, None),
          Author(Some("St. Leger"), Seq("R", "J"), None, None)
        ), false))
      }

      * - {
        val txt = "Vu.J., Vujicic,M., Church,D.M. and"
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths.authors.size == 3)
      }

      * - {
        val txt = "Murray,J., Sheffield,V, Weber,J.L., Duyk,G. and Buetow,K.H.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Murray"), Seq("J"), None, None),
          Author(Some("Sheffield"), Seq("V"), None, None),
          Author(Some("Weber"), Seq("J", "L"), None, None),
          Author(Some("Duyk"), Seq("G"), None, None),
          Author(Some("Buetow"), Seq("K", "H"), None, None)
        ), false))
      }

      * - {
        val txt = "Rana,S., Shveta, Singh,D.P.,"
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Rana"), Seq("S"), None, None),
          Author(Some("Shveta"), Seq(), None, None),
          Author(Some("Singh"), Seq("D", "P"), None, None)
        ), false))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Clarridge"), Seq("J", "E"), Some("III"), None)
        ), false))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Clarridge"), Seq("J", "E"), Some("III"), None),
          Author(Some("Cookson"), Seq("B", "T"), None, None)
        ), false))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Arthi"), Seq("S"), None, None))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Arthi"), Seq("S"), None, None), Author(Some("Shobana"), Seq(), None, None), Author(Some("Ananthi"), Seq(), None, None)), false))
      }

      * - {
        val txt = "Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Gala")
      }

      * - {
        val txt = "Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Gala"), Seq("J", "-L"), None, None))
      }

      * - {
        val txt = "Philippe,M. and Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Philippe"), Seq("M"), None, None), Author(Some("Gala"), Seq("J", "-L"), None, None)), false))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Valli Nachiyar")
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Valli Nachiyar"), Seq(), None, None))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Valli Nachiyar"), Seq(), None, None))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(Author(Some("Valli Nachiyar"), Seq(), None, None)), false))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Valli Nachiyar"), Seq(), None, None), Author(Some("Rose"), Seq("C"), None, None)), false))
      }

      * - {
        val txt = "Frere,J.M. et al.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Frere"), Seq("J", "M"), None, None)), true))
      }

      * - {
        val txt = "Singh,J.A, Sprott,D. and Tinker,N.A."
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Singh"), Seq("J", "A"), None, None))
      }

      * - {
        val txt = "Wang,L.H.,,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Wang"), Seq("L", "H"), None, None))
      }

      * - {
        val txt = "O' TOOLE,M.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("O' TOOLE"), Seq("M"), None, None))
      }

      * - {
        val txt = "O'BRIEN,A.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("O'BRIEN"), Seq("A"), None, None))
      }

      * - {
        val txt = "IBARROLA LOPEZ DE DAVALILLO,I.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("IBARROLA LOPEZ DE DAVALILLO"), Seq("I"), None, None))
      }

      * - {
        val txt = "GARCIA LOPEZ ERNESTO,[.E.S.].,"
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("GARCIA LOPEZ ERNESTO"), Seq(), None, Some("ES")))
      }

      * - {
        val txt = "GARCIA LOPEZ ERNESTO,[.E.S.].,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("GARCIA LOPEZ ERNESTO"), Seq(), None, Some("ES")))
      }

      * - {
        val txt = "CARY STEPHEN P L,[.U.S.].,"
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("CARY STEPHEN P L"), Seq(), None, Some("US")))
      }

      * - {
        val txt = "CARY STEPHEN P L,[.U.S.].,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("CARY STEPHEN P L"), Seq(), None, Some("US")))
      }

      * - {
        val txt = "VAN TRUNG CHU,(.V.N.).\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("VAN TRUNG CHU"), Seq("(", "V", "N", ")"), None, None))
      }

      * - {
        val txt = "PALAZ0N GARCIA,F.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("PALAZ0N GARCIA"), Seq("F"), None, None))
      }

      * - {
        val txt = "O'BRIEN,A.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("O'BRIEN"), Seq("A"), None, None))
      }

      * - {
        val txt = "O'BRIEN SIMPSON,N.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("O'BRIEN SIMPSON"), Seq("N"), None, None))
      }

      * - {
        val txt = "GLA?KLER,J. and MERTES,F."
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("GLA?KLER"), Seq("J"), None, None))
      }

      * - {
        val txt = "Genoscope.\n"
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Genoscope"), Seq(), None, None))
      }

      * - {
        val txt = "Genoscope.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Genoscope"), Seq(), None, None))
      }

      * - {
        val txt = "Genoscope.\n"
        val auths = parse(GenbankParser.SingleAuthor, txt)
        assert(auths == Author(Some("Genoscope"), Seq(), None, None))
      }

      * - {
        val txt = "F.San.,"
        val auths = parse(GenbankParser.InitialsWithNamesIn, txt)
        assert(auths == Seq("F", "San"))
      }

      * - {
        val txt = "Lucas,F.San.,"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("Lucas"), Seq("F", "San"), None, None))
      }

      * - {
        val txt = "Lucas,F.San.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Lucas"), Seq("F", "San"), None, None))
      }

      * - {
        val txt = "Genoscope -,C.E.A.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Genoscope -"), Seq("C", "E", "A"), None, None))
      }

      * - {
        val txt = "Genoscope -,C.E.A.\n"
        val auths = parse(GenbankParser.SingleAuthor, txt)
        assert(auths == Author(Some("Genoscope -"), Seq("C", "E", "A"), None, None))
      }

      * - {
        val txt = "F,G.Gao.,"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("F"), Seq("G", "Gao"), None, None))
      }

      * - {
        val txt = "F,G.Gao.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("F"), Seq("G", "Gao"), None, None))
      }

      * - {
        val txt = "McSpadden Gardener,B.B."
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "McSpadden Gardener")
      }

      * - {
        val txt = "McSpadden Gardener,B.B."
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "McSpadden Gardener")
      }

      * - {
        val txt = "McSpadden Gardener,B.B.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("McSpadden Gardener"), Seq("B", "B"), None, None))
      }

      * - {
        val txt = "Shen,C.-h.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Shen"), Seq("C", "-h"), None, None))
      }

      * - {
        val txt = "Leverstein-van Hall,M.A.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Leverstein-van Hall"), Seq("M", "A"), None, None))
      }

      * - {
        val txt = "Folly,Y.Minnie.Elodie.,"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("Folly"), Seq("Y", "Minnie", "Elodie"), None, None))
      }

      * - {
        val txt = "Folly,Y.Minnie.Elodie.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Folly"), Seq("Y", "Minnie", "Elodie"), None, None))
      }

      * - {
        val txt = "Andreichuk,Iu.V.,"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("Andreichuk"), Seq("Iu", "V"), None, None))
      }

      * - {
        val txt = "Andreichuk,Iu.V.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Andreichuk"), Seq("Iu", "V"), None, None))
      }

      * - {
        val txt = "Kharroub,k.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Kharroub"), Seq("k"), None, None))
      }

      * - {
        val txt = "Z.(.\n"
        val auths = parse(GenbankParser.Initials, txt)
        assert(auths == Seq("Z", "("))
      }

      * - {
        val txt = "Wang,Z.(.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Wang"), Seq("Z", "("), None, None))
      }

      * - {
        val txt = "s Rensen,A.M.,"
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "s Rensen")
      }

      * - {
        val txt = "s Rensen,A.M.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("s Rensen"), Seq("A", "M"), None, None))
      }

      * - {
        val txt = "sim n Buela,L.,"
        val auths = parse(GenbankParser.CompoundNamePrefix, txt)
        assert(auths == "sim")
      }

      * - {
        val txt = "sim n Buela,L.,"
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "sim n Buela")
      }

      * - {
        val txt = "sim n Buela,L.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("sim n Buela"), Seq("L"), None, None))
      }

      * - {
        val txt = "Egashira,K.1.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Egashira"), Seq("K", "1"), None, None))
      }

      * - {
        val txt = "R,S.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("R"), Seq("S"), None, None))
      }

      * - {
        val txt = "Espinsa -Ruiz,M.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Espinsa -Ruiz"), Seq("M"), None, None))
      }

      * - {
        val txt = "[SI],"
        val auths = parse(GenbankParser.CountryCode, txt)
        assert(auths == "SI")
      }

      * - {
        val txt = "Ea??[SI],"
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Ea??")
      }

      * - {
        val txt = "Ea??[SI],"
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Ea??"), Seq(), None, Some("SI")))
      }

      * - {
        val txt = "Ea??[SI],"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Ea??"), Seq(), None, Some("SI")))
      }

      * - {
        val txt = "STRA&ZCARON,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("STRA&ZCARON"), Seq(), None, None))
      }

      * - {
        val txt = "Y.\n"
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Y"), Seq(), None, None))
      }

      * - {
        val txt = "Y.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Y"), Seq(), None, None))
      }

      * - {
        val txt = "Hoosen A.A.,"
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Hoosen")
      }

      * - {
        val txt = "Hoosen A.A.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Hoosen"), Seq("A", "A"), None, None))
      }

      * - {
        val txt = "Reyes1,J.D.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Reyes1"), Seq("J", "D"), None, None))
      }

      * - {
        val txt = "Lajudie,P. de,"
        val auths = parse(GenbankParser.WellFormedAuthor, txt)
        assert(auths == Author(Some("Lajudie"), Seq("P"), Some("de"), None))
      }

      * - {
        val txt = "Lajudie,P. de,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Lajudie"), Seq("P"), Some("de"), None))
      }

      * - {
        val txt = "Sena-D Anna,L.,"
        val auths = parse(GenbankParser.CompoundNamePrefix, txt)
        assert(auths == "Sena-D")
      }

      * - {
        val txt = "Sena-D Anna,L.,"
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "Sena-D Anna")
      }

      * - {
        val txt = "Sena-D Anna,L.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Sena-D Anna"), Seq("L"), None, None))
      }

      * - {
        val txt = """Crop
                    |            Plant Research (IPK).""".stripMargin
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "Crop Plant Research (IPK)")
      }

      * - {
        val txt = """Crop
                    |            Plant Research (IPK).
                    |            """.stripMargin
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Crop Plant Research (IPK)"), Seq(), None, None))
      }

      * - {
        val txt = """Crop
                    |            Plant Research (IPK).
                    |            """.stripMargin
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Crop Plant Research (IPK)"), Seq(), None, None))
      }

      * - {
        val txt = "O`Reilly,L.D.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("O`Reilly"), Seq("L", "D"), None, None))
      }

      * - {
        val txt = """Vinals
                    |            Y De Bassols,C.
                    |""".stripMargin
        val auths = parse(GenbankParser.CompoundName, txt)
        assert(auths == "Vinals Y De Bassols")
      }

      * - {
        val txt = """Vinals
                    |            Y De Bassols,C.
                    |""".stripMargin
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Vinals Y De Bassols"), Seq("C"), None, None))
      }

      * - {
        val txt = "Haedrich,(b.Moeckel.).B.,"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("Haedrich"), Seq("(b", "Moeckel", ")", "B"), None, None))
      }

      * - {
        val txt = "Haedrich,(b.Moeckel.).B.,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Haedrich"), Seq("(b", "Moeckel", ")", "B"), None, None))
      }

      * - {
        val txt = "M.o slashed.ller,Sslashedren.,\n"
        val auths = parse(GenbankParser.InitialsWithNamesIn, txt, Some("M.o slashed.ller,"))
        assert(auths == Seq("M", "o slashed", "ller"))
      }

      * - {
        val txt = "M.o slashed.ller,Sslashedren.,\n"
        val auths = parse(GenbankParser.InitialsWithNamesInThenName, txt, Some("M.o slashed.ller,Sslashedren."))
        assert(auths == Author(Some("Sslashedren"), Seq("M", "o slashed", "ller"), None, None))
      }

      * - {
        val txt = "M.o slashed.ller,Sslashedren.,\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Sslashedren"), Seq("M", "o slashed", "ller"), None, None))
      }

      * - {
        val txt = "M.Eskelund., "
        val auths = parse(GenbankParser.CompoundNamePrefix, txt)
        assert(auths == "M.")
      }

      * - {
        val txt = "M.Eskelund., "
        val auths = parse(GenbankParser.BrokenName, txt)
        assert(auths == "M.Eskelund.")
      }

      * - {
        val txt = "Bj.o\n            slashed.rnvad,M.Eskelund., "
        val auths = parse(GenbankParser.InitialsWithNamesIn, txt, Some("Bj.o\n            slashed.rnvad,"))
        assert(auths == Seq("Bj", "o slashed", "rnvad"))
      }

      * - {
        val txt = "Bj.o\n            slashed.rnvad,M.Eskelund., "
        val auths = parse(GenbankParser.InitialsWithNamesInThenName, txt)
        assert(auths == Author(Some("M.Eskelund."), Seq("Bj", "o slashed", "rnvad"), None, None))
      }

      * - {
        val txt = "Bj.o\n            slashed.rnvad,M.Eskelund., "
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("M.Eskelund."), Seq("Bj", "o slashed", "rnvad"), None, None))
      }

      * - {
        val txt = "3rd, "
        val auths = parse(GenbankParser.NameNumber, txt)
        assert(auths == "3rd")
      }

      * - {
        val txt = "Plunkett,G. 3rd, "
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Plunkett"), Seq("G"), Some("3rd"), None))
      }

      * - {
        val txt = "M.>.\n"
        val auths = parse(GenbankParser.Initials, txt)
        assert(auths == Seq("M", ">"))
      }

      * - {
        val txt = "VVyssotski,M.>.\n"
        val auths = parse(GenbankParser.WellFormedAuthor, txt)
        assert(auths == Author(Some("VVyssotski"), Seq("M", ">"), None, None))
      }

      * - {
        val txt = "VVyssotski,M.>.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("VVyssotski"), Seq("M", ">"), None, None))
      }

      * - {
        val txt = "R.B.Gennis.\n"
        val auths = parse(GenbankParser.InitialsThenName, txt)
        assert(auths == Author(Some("Gennis"), Seq("R", "B"), None, None))
      }

      * - {
        val txt = "R.B.Gennis.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Gennis"), Seq("R", "B"), None, None))
      }

      * - {
        val txt = "3rd,R.C. and"
        val auths = parse(GenbankParser.NameNumberThenInitials, txt)
        assert(auths == Author(None, Seq("R", "C"), Some("3rd"), None))
      }

      * - {
        val txt = "3rd,R.C. and"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(None, Seq("R", "C"), Some("3rd"), None))
      }

      * - {
        val txt = "Y.Mori.K.Hatada.Y.\n"
        val auths = parse(GenbankParser.InitialsWithNamesIn, txt)
        assert(auths == Seq("Y", "Mori", "K", "Hatada", "Y"))
      }

      * - {
        val txt = "Nogi,Y.Mori.K.Hatada.Y.\n"
        val auths = parse(GenbankParser.NameWithNamesInInitials, txt)
        assert(auths == Author(Some("Nogi"), Seq("Y", "Mori", "K", "Hatada", "Y"), None, None))
      }

      * - {
        val txt = "Nogi,Y.Mori.K.Hatada.Y.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Nogi"), Seq("Y", "Mori", "K", "Hatada", "Y"), None, None))
      }

      * - {
        val txt = "Haq,I.-U.-. and"
        val auths = parse(GenbankParser.WellFormedAuthor, txt)
        assert(auths == Author(Some("Haq"), Seq("I", "-U", "-"), None, None))
      }

      * - {
        val txt = "Haq,I.-U.-. and"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Haq"), Seq("I", "-U", "-"), None, None))
      }

    }

    'authors {
      * - {
          val txt =
          """  AUTHORS   McPherson,J.D., Apostol,B., Wagner-McPherson,C.B., Hakim,S., Del
            |            Mastro,R.G., Aziz,N., Baer,E., Gonzales,G., Krane,M.C.,
            |            Markovich,R., Masny,P., Ortega,M., Vu.J., Vujicic,M., Church,D.M.,
            |            Segal,A., Grady,D.L., Moyzis,R.K., Spence,M.A., Lovett,M. and
            |            Wasmuth,J.J.
            |""".stripMargin

        parse(GenbankParser.ReferenceAuthors ~ End, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Duan,J.B., Zou,M.J., Zhang,B.L., Cai,X., Wang,L.H.,, Li,Y.Z. and
                    |            Wang,J.X.
                    |  TITLE     Endostatin-responsive genes in rat brain micro-vascular endothelia
                    |""".stripMargin

        val auth = parse(GenbankParser.ReferenceAuthors, txt)
        assert(
          auth.authors.size == 7)
      }

      * - {
        val txt =
          """  AUTHORS   Dias Neto,E., Garcia Correa,R., Verjovski-Almeida,S., Briones,M.R.,
            |            Nagai,M.A., da Silva,W. Jr., Zago,M.A., Bordin,S., Costa,F.F.,
            |            Goldman,G.H., Carvalho,A.F., Matsukuma,A., Baia,G.S., Simpson,D.H.,
            |            Brunstein,A., deOliveira,P.S., Bucher,P., Jongeneel,C.V.,
            |            O'Hare,M.J., Soares,F., Brentani,R.R., Reis,L.F., de Souza,S.J. and
            |            Simpson,A.J.
            |  TITLE     Shotgun sequencing of the human transcriptome with ORF expressed
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)

      }

      * - {
        val txt =
          """  AUTHORS   Kaur,S., Singh,Y., Rana,S., Shveta, Singh,D.P., Khattar,J.I.S. and
            |            Gulati,A.
            |  TITLE     Phycobiliproteins production from the cyanobacterium Anabaena
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)

      }

      * - {
        val txt =
          """  AUTHORS   Harrington,A.T., Castellanos,J.A., Ziedalski,T.M., Clarridge,J.E.
            |            III and Cookson,B.T.
            |  TITLE     Isolation of Bordetella avium and novel Bordetella strain from
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)

      }

      * - {
        val txt =
          """  AUTHORS   Kuramoto,T., Kitada,K., Inui,T., Sasaki,Y., Ito,K., Hase,T.,
                    |            Kawagachi,S., Ogawa,Y., Nakao,K., Barsh,G.S., Nagao,M., Ushijima,T.
                    |            and Serikawa,T.
                    |  TITLE     Attractin/mahogany/zitter plays a critical role in myelination of""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Eyini, Arun,A., Arthi,S., Shobana and Ananthi.
            |  TITLE     Enrichment and isolation of PAHs biodegrading and high
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Shaw,G.R., Sukenik. A., Livne,A., Chiswell,R.K., Smith,M.J.,
            |            Seawright,A.A., Norris,R.L., Eaglesham,G.K. and Moore,M.R.
            |  TITLE     Blooms of the Hepatotoxic Cyanobacterium, Aphanizomenon ovalisporum
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Vannuffel,P., Heusterspreute,M., Bouyer,M., Philippe,M. and Gala
            |            J.-L.
            |  TITLE     Molecular characterization of femA from Staphylococcus hominis,
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   L.L. Richardson, W.M. Goldberg, K.G. Kuta, R.B.Aronson, G.W. Smith,
            |            K.B. Ritchie, J.C. Halas, J.S. Feingold and S.L. Miller.
            |  TITLE     Florida's mystery coral killer identified
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Anuradha Jabasingh,S., Ananth,P.B., Karunya,A., Nixon Raj,N., Valli
            |            Nachiyar and Rose,C.
            |  TITLE     Isolation and characterization of dye degrading bacteria from
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Schell,M.,A.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Timm,J., Perilli,M.G., Duez,C., Trias,J., Orefici,G., Fattorini,L.,
            |            Amicosante,G., Oratore,A., Joris,B., Frere,J.M. et al.
            |  TITLE     Transcription and expression analysis, using lacZ and phoA gene
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Shaw,G.R., Sukenik. A., Livne,A., Chiswell,R.K., Smith,M.J.,
            |            Seawright,A.A., Norris,R.L., Eaglesham,G.K. and Moore,M.R.
            |  TITLE     Blooms of the Hepatotoxic Cyanobacterium, Aphanizomenon ovalisporum
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Ouellet,T., Procunier,D., Prashar,S., Dan,H., Chapados,J.,
            |            Couroux,P., De Moors,A., Harris,L.J., Hattori,J.I., Robert,L.S.,
            |            Singh,J.A, Sprott,D. and Tinker,N.A.
            |  TITLE     Expressed Sequence Tags from Wheat Heads 24 Hours after Spray
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Walbot,V.
            |  TITLE     Maize ESTs from various cDNA libraries sequenced at Stanford
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   GARCIA LOPEZ ERNESTO,[.E.S.]., GARCIA GONZALEZ PEDRO,[.E.S.].,
            |            GARCIA LOPEZ JOSE LUIS,[.E.S.]., CAMPUZANO RUIZ SUSANA,[.E.S.].,
            |            MORALES AREIZAGA MARIA,[.E.S.]., ARDUNAY TISAIRE MARIA
            |            CARMEN,[.E.S.]., PINGARRON CARRAZON JOSE MANUEL,[.E.S.]. and
            |            PEDRERO MUNOZ MARIA,[.E.S.].
            |  TITLE     DETECTION OF STREPTOCOCCUS PNEUMONIAE THROUGH MAGNETO-AMPEROMETRIC
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   BEATTIE,C., YAMADA,T. and DAS GUPTA,T.
            |  TITLE     COMPOSITIONS AND METHODS TO PREVENT CANCER BY STABILIZING P53
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   FOUSER,L., HEGEN,M., LUXENBERG,D. and O' TOOLE,M.
            |  TITLE     Human antibodies against human interleukin-22 (IL-22)
            | """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   ASTURIAS ORTEGA,J., IBARROLA LOPEZ DE DAVALILLO,I., ARILLA
            |            RODRIGUEZ,M. and MARTINEZ GARATE,A.
            |  TITLE     Hypoallergenic hybrid proteins of major group 1 and 2 mite
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   KLINGUER-HAMOUR CHRISTINE,[.F.R.]. and JOUHANNEAUD
            |            ALEXANDRA,[.F.R.].
            |  TITLE     USE OF THE ANTIBODY I-3859 FOR THE DETECTION AND DIAGNOSIS OF
            | """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   CARY STEPHEN P L,[.U.S.]., BOON ELIZABETH M,[.U.S.]., WEINERT
            |            EMILY,[.U.S.]., WINGER JONATHAN A,[.U.S.]. and MARLETTA MICHAEL
            |            A,[.U.S.].
            |  TITLE     COMPOSITIONS AND METHODS FOR THE DELIVERY OF OXYGEN
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   BEREK,C. and VAN TRUNG CHU,(.V.N.).
            |  TITLE     Eosinophils as a therapeutic target
            | """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   BERRAONDO LOPEZ,P., FIORAVANTI,J., MEDINA ECHEVERZ,J., MELERO
            |            BERMEJO,I., OCHOA NIETO,M., PALAZ0N GARCIA,F., BULFONE-PAUS,S. and
            |            DUITMAN,E.H.
            |  TITLE     NOVEL CONJUGATES AND COMPOSITIONS FOR IMMUNOTHERAPY AND
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   O'BRIEN,A., MELTON-CELSA,A., SMITH,M. and SINCLAIR,J.
            |  TITLE     METHODS AND COMPOSITIONS BASED ON SHIGA TOXIN TYPE 2 PROTEIN
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   REYNOLDS,E., O'BRIEN SIMPSON,N., CROSS,K. and SLAKESKI,N.
            |  TITLE     PREVENTION, TREATMENT AND DIAGNOSIS OF P.GINGIVALIS INFECTION
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   GLA?KLER,J. and MERTES,F.
            |  TITLE     Low melting temperature primer
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Genoscope.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Muzny,D.M., Qin,X., Buhay,C.J., Dugan-Rocha,S., Ding,Y., Chen,G.,
            |            Hawes,A.C., Holder,M., Jhangiani,S.N., Johnson,A.J., Khan,Z.M.,
            |            Li,Z., Liu,W., Liu,X., Perez,L.M., Shen,H., Wang,Q., Watt,J.E.,
            |            Xi,L., Xin,Y., Zhou,J., Deng,J., Jiang,H., Liu,Y., Qu,J.,
            |            Song,X.-Z., Zhang,L., Villasana,D., Liu,J., Liyanage,D.,
            |            Lorensuhewa,L.M., Robinson,T., Song,A., Song,B.-B., Dinh,H.H.,
            |            Thornton,R., Coyle,M.D., Francisco,L., Jackson,L., Javaid,M.,
            |            Korchina,V., Kovar,C.L., Mata,R., Mathew,T., Ngo,R., Nguyen,L.,
            |            Nguyen,N., Okwuonu,G., Ongeri,F., Pham,C., Simmons,D.,
            |            Wilczek-Boney,K.B., Hale,W., Jakkamsetti,A., Opheim,D., Pham,P.,
            |            Lucas,F.San., Warren,J., Zhang,J., Zhao,Z., Zhou,C., Zhu,D.,
            |            Lee,S.L., Bess,C.M., Blankenburg,K.P., Forbes,L., Fu,Q.,
            |            Gubbala,S., Hirani,K., Jayaseelan,J.C., Lara,F., Munidasa,M.,
            |            Palculict,T., Patil,S.S., Pu,L.-L., Saada,N., Tang,L.-Y.,
            |            Weissenberger,G.M., Zhu,Y., Hemphill,L., Shang,Y., Youmans,B.,
            |            Ayvaz,T., Ross,M., Santibanez,J., Aqrawi,P., Gross,S., Joshi,V.,
            |            Fowler,G., Nazareth,L., Reid,J., Worley,K.C., Petrosino,J.,
            |            Highlander,S. and Gibbs,R.A.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Genoscope -,C.E.A.
            |  CONSRTM   1:UMR Genomique Developpement Pouvoir Pathogene INRA, 33883
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Aram,M.A., Markande,A.R., Ketan,P.D., Niyati,V.B., Vennila,A.,
            |            Bhushan,N.Binay., Nerurkar,A.S. and Purushothaman,C.S.
            |  TITLE     Molecular and Cultural Analysis of Nereis chilkaensis (Southern)
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Chen,C., Tang,J., Dong,W., Wang,J., Wang,C., Pan,X., Zheng,F.,
            |            Song,Y., Dong,Y., Zhu,X., Sun,H., Feng,T., Guo,Z., Ju,A., Ge,J.,
            |            Wang,J., Wang,X., F,G.Gao., Yang,H., Yang,R., Wang,J. and Yu,J.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Joshi,R. and McSpadden Gardener,B.B.
            |  TITLE     Identification and Characterization of Novel Genetic Markers
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Yang,M.-K., Lin,Y.-C. and Shen,C.-h.
            |  TITLE     Identification of two gene loci involved in
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Paauw,A., Fluit,A.C., Verhoef,J. and Leverstein-van Hall,M.A.
            |  TITLE     Enterobacter cloacae outbreak and emergence of quinolone resistance
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Zhao,Y., Sangare,L., Wang,Y., Folly,Y.Minnie.Elodie.,
            |            Selvaraj,J.Nimal. and Liu,Y.
            |  TITLE     Complete genome sequence of Bacillus subtilis SG6 antagonistic
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Fadeeva,I.A., Korenberg,E.I., Nefedova,V.V., Andreichuk,Iu.V.,
            |            Markov,A.V. and Shaginian,I.A.
            |  TITLE     [Genetic heterogeneity of Borrelia afzelii in the natural focus of
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Kharroub,K., Quesada,T., Ferrer,R., Fuentes,S., Aguilera,M.,
            |            Boulahrouf,A., Ramos-Cormenzana,A. and Monteoliva-Sanchez,M.
            |  TITLE     Halorubrum ezzemoulense sp. nov., a halophilic archaeon isolated
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Kharroub,K., Monteoliva-Sanchez,M., Ramos-Cormenzana,A. and
            |            Boulahrouf,A.
            |  TITLE     Halorubrum ezzemoulense sp. nov., an novel halophilic archaeon
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Kharroub,k., Monteoliva-Sanchez,M., Ramos-Cormenzana,A. and
            |            Boulahrouf,A.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Jia,K. V and Li,S. Sr.
            |  TITLE     Isolation and characterization of nitrogen fixating and
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   .
            |  TITLE     Enhanced expression of fusion polypeptides with a biotinylation tag
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Wang,Z.(.
            |  TITLE     Mthp promoter element
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   elm N,J., Wahlestedt,C., Liang,Z., s Rensen,A.M., Rum,H. and
            |            Koch,T.
            |  TITLE     SHORT INTERFERING RNA (siRNA) ANALOGUES
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   martinez Martinez,A., sim n Buela,L., santa Cruz,S., s enz jim
            |            Nez,M.P., molina Vila,M., Junquera,S.N., g mez rom N,J.J. and
            |            cuevas gonz Lez,J.
            |  TITLE     In vitro method to detect bladder transitional cell carcinoma
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Raut,A., R,S., Vaishampain,P., Souchey,Y., Bandekar,J. and
            |            Kapadnis,B.
            |  TITLE     Partial 16S rRNA sequence of a gram negative poultry isolate from
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Revol,A., Espinsa -Ruiz,M., Medina-Villanueva,I. and
            |            Salinas-Carmona,M.C.
            |  TITLE     Expression of Nocardia brasiliensis superoxide dismutase during the
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   JERALA,R., BENCINA,M., MAJERLE,A., OBLAK,A., LEBAR,T.,
            |            FORSTNERIC,V., LONZARIC,J., SMOLE,A., Ea??[SI], GABER,R.,
            |            BEZELJAK,U., GOLOB,A., KADUNC,L., VUCKO,D., STRA&ZCARON, AR,M.,
            |            PIRS,B., JERALA,M., ZUPANCIC,U., SOMRAK,M., LU&ZCARON and NIK,Z.
            |  TITLE     BISTABLE GENETIC TOGGLE SWITCH COMPRISING A PAIR OF RECIPROCAL
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Liu,X.D., Xu,Y. and Y.
            |  TITLE     Molecular cloning and characterization of an alpha-amylase with raw
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Kharsany,A.B.M., Hoosen A.A., Kiepiela P. and Sturm A.W.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Reyes1,J.D., Tabche1,M., Morera,C., Girard,M.L., Romero,D.,
            |            Krol,E., Miranda,J. and Soberon,M.
            |  TITLE     Expression pattern of Rhizobium etli ccmIEFH genes involved in
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Sy,A., Giraud,E., Jourand,P., Willems,A., Lajudie,P. de, Samba,R.,
            |            Prin,Y., Neyra,M., Gillis,M., Masson-Boivin,C. and Dreyfus,B.
            |  TITLE     Direct Submission
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Pannekoek,Y., Huis In 't Veld,R., Hopman,C.T., Langerak,A.A.,
            |            Speijer,D. and van der Ende,A.
            |  TITLE     Molecular characterization and identification of proteins regulated
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Sena-D Anna,L., Moret,J., Gonzalez,H., Rojas-Tortolero,D.,
            |            Montiel,E. and Naranjo-Briceno,L.
            |  TITLE     A strategy to obtain axenic cultures of the cyanobacterium
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   European Union Chromosome 3 Arabidopsis Genome Sequencing
            |            Consortium, The Institute for Genomic Research and Kazusa DNA
            |            Research Institute.
            |  TITLE     Sequence and analysis of chromosome 3 of the plant Arabidopsis
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Kazusa DNA Research Institute, The Cold Spring Harbor, Washington
            |            University Sequencing Consortium, The European Union Arabidopsis
            |            Genome Sequencing Consortium, Institute of Plant Genetics and Crop
            |            Plant Research (IPK).
            |  TITLE     Sequence and analysis of chromosome 5 of the plant Arabidopsis
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Cabezon Silva,T.E., Cassart,J.P., Coche,T., Gaulis,S.R. and Vinals
            |            Y De Bassols,C.
            |  TITLE     Novel compounds
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   The FANTOM Consortium and RIKEN Genome Exploration Research Group
            |            and Genome Science Group (Genome Network Project Core Group).
            |  TITLE     The transcriptional landscape of the mammalian genome
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Andersen,L.Nonboe., Schulein,M., Lange,N.Erik.Krebs., Bj.o
            |            slashed.rnvad,M.Eskelund., M.o slashed.ller,Sslashedren.,
            |            Glad,S.O.Schroslashedder., Kauppinen,M.Sakari., Schnorr,K. and
            |            Kongsbak,L.
            |  TITLE     Pectate lyases
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Riley,M., Abe,T., Arnaud,M.B., Berlyn,M.K., Blattner,F.R.,
            |            Chaudhuri,R.R., Glasner,J.D., Horiuchi,T., Keseler,I.M., Kosuge,T.,
            |            Mori,H., Perna,N.T., Plunkett,G. 3rd, Rudd,K.E., Serres,M.H.,
            |            Thomas,G.H., Thomson,N.R., Wishart,D. and Wanner,B.L.
            |  TITLE     Escherichia coli K-12: a cooperatively developed annotation
          """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Lagutin,K., MacKenzie,A., Houghton,K.M., Stott,M.B. and
            |            VVyssotski,M.>.
            |  TITLE     Phospholipids of Thermus and Meiothermus bacteria and its
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   R.B.Gennis.
            |  TITLE     Direct Submission
          """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Stacy,A.K., Mitchell,N.M., Maddux,J.T., De la Cruz,M.A., Duran,L.,
            |            Giron,J.A., 3rd,R.C. and Mellata,M.
            |  TITLE     Evaluation of the Prevalence and Production of Escherichia coli
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Nogi,Y.Mori.K.Hatada.Y.
            |  TITLE     Thalassobius abyssi sp. nov., a marine bacterium isolated from the
            | """.stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }

      * - {
        val txt =
          """  AUTHORS   Hameed,U., Haq,I.-U.-. and Wilson,D.B.
            |  TITLE     Cloning and expression of alpha amylase catalytic domain from
            |""".stripMargin
        parse(GenbankParser.ReferenceAuthors, txt)
      }
    }

    'referenceReference {
      * - {
        val ref = "REFERENCE   2  (bases 1 to 13033779)\n"
        parse(GenbankParser.ReferenceReference, ref)
      }

      * - {
        val ref = "REFERENCE   2  (bases 1 to 13033779; 200 to 3455)\n"
        parse(GenbankParser.ReferenceReference, ref)
      }

      * - {
        val ref = "REFERENCE   3  (sites)\n"
        parse(GenbankParser.ReferenceReference, ref)
      }

      * - {
        val ref =
          """REFERENCE   3  (bases 366 to 3925; 6524 to 9071; 15809 to 17312; 20127 to
                    |            21187; 21464 to 22350; 22451 to 22836)
                    |""".stripMargin
        parse(GenbankParser.ReferenceReference, ref)
      }

      * - {
        val ref = "REFERENCE   1\n"
        parse(GenbankParser.ReferenceReference, ref)
      }
    }

    'referenceTitle {

      * - {
        val title = "  TITLE     Novel compounds\n"
        parse(GenbankParser.ReferenceTitle, title)
      }
    }

    'referenceJournal {

      * - {
        val journal = """  JOURNAL   Patent: EP 1650221-A2 1 26-APR-2006;
                        |            GlaxoSmithKline Biologicals SA (BE)
                        |FEATURES             Location/Qualifiers""".stripMargin
        parse(GenbankParser.ReferenceJournal, journal)
      }

    }

    'reference {

      * - {
        val ref = """REFERENCE   1  (bases 1 to 4418548)
                    |  AUTHORS   Earl,A.M., Galagan,J., Murray,M., Pillay,M., Borowsky,M.L.,
                    |            Young,S., Zeng,Q., Gargeya,S., Fitzgerald,M., Abouelleil,A.,
                    |            Alvarado,L., Chapman,S.B., Dunbar,C., Gainer-Dewar,J., Goldberg,J.,
                    |            Griggs,A., Gujja,S., Hansen,M., Howarth,C., Imamovic,A.,
                    |            Larimer,J., Montmyeur,A., Murphy,C., Naylor,J., Pearson,M.,
                    |            Poon,T.W., Priest,M., Roberts,A., Saif,S., Shea,T., Sykes,S.,
                    |            Wortman,J., Nusbaum,C. and Birren,B.
                    |  CONSRTM   The Broad Institute Genomics Platform, The Broad Institute Genome
                    |            Sequencing Center for Infectious Disease
                    |  TITLE     The Genome Sequence of Mycobacterium tuberculosis W-148
                    |  JOURNAL   Unpublished
                    |REFERENCE   2  (bases 1 to 4418548)
                    |""".stripMargin

        val r = parse(GenbankParser.Reference, ref)

        assert(r.consortium.size > 0)
      }

      * - {
        val ref = """REFERENCE   1
                    |  AUTHORS   Constantijn,B.Mennes.C.M., Michelangelo,S.Moerland.M.M., Magnus
                    |            Rath,M.R., Erik,F.Smets.E.S. and Vincent,S.F.T.Merckx.V.M.
                    |  TITLE     Evolution of mycoheterotrophy in Polygalaceae: the case of
                    |            Epirixanthes
                    |  JOURNAL   Unpublished
                    |REFERENCE   2  (bases 1 to 496)
                    |""".stripMargin
        val r = parse(GenbankParser.Reference, ref)
      }

      * - {
        val ref = """REFERENCE   1  (bases 1 to 360)
                    |  AUTHORS   Bosio,C.F., Fulton,R.E., Salasek,M.L., Beaty,B.J. and Black,W.C.
                    |            IV.
                    |  TITLE     Quantitative trait loci that control vector competence for dengue-2
                    |            virus in the mosquito Aedes aegypti
                    |  JOURNAL   Genetics 156 (2), 687-698 (2000)
                    |   PUBMED   11014816
                    |FEATURES             Location/Qualifiers
                    |""".stripMargin
        val r = parse(GenbankParser.Reference, ref)
      }

      * - {
        val ref = """REFERENCE   2  (bases 1 to 1995281)
                    |  AUTHORS   Beare,P.A., Unsworth,N., Andoh,M., Voth,D.E., Omsland,A.,
                    |            Gilk,S.D., Williams,K.P., Sobral,B.W., Kupko,J.J. III,
                    |            Porcella,S.F., Samuel,J.E. and Heinzen,R.A.
                    |  TITLE     Comparative genomics reveal extensive transposon-mediated genomic
                    |            plasticity and diversity among potential effector proteins within
                    |            the genus Coxiella
                    |  JOURNAL   Infect. Immun. 77 (2), 642-656 (2009)
                    |   PUBMED   19047403
                    |REFERENCE   3  (bases 1 to 1995281)
                    |""".stripMargin
        val r = parse(GenbankParser.Reference, ref)
      }

      * - {
        val ref = """REFERENCE   1  (bases 1 to 432)
                    |  AUTHORS   Ouellet,T., Procunier,D., Prashar,S., Dan,H., Chapados,J.,
                    |            Couroux,P., De Moors,A., Harris,L.J., Hattori,J.I., Robert,L.S.,
                    |            Singh,J.A, Sprott,D. and Tinker,N.A.
                    |  TITLE     Expressed Sequence Tags from Wheat Heads 24 Hours after Spray
                    |            Inoculation with Fusarium graminearum (part 2)
                    |  JOURNAL   Unpublished (2002)
                    |COMMENT     Contact: Ouellet, Therese
                    |""".stripMargin
        val r = parse(GenbankParser.Reference, ref)
      }

      * - {
        val ref = """REFERENCE   1
                    |  AUTHORS   Cabezon Silva,T.E., Cassart,J.P., Coche,T., Gaulis,S.R. and Vinals
                    |            Y De Bassols,C.
                    |  TITLE     Novel compounds
                    |  JOURNAL   Patent: EP 1650221-A2 1 26-APR-2006;
                    |            GlaxoSmithKline Biologicals SA (BE)
                    |FEATURES             Location/Qualifiers
                    |""".stripMargin
        val r = parse(GenbankParser.Reference, ref)
      }
    }

  }
  def parse[T](p: Parser[T], txt: String, consumed: Option[String] = None): T = {
    p.parse(txt) match {
      case Success(t, i) =>
        consumed match {
          case None => t
          case Some(str) =>
            val matchStr = txt.substring(0, i)
            if(str == matchStr) t
            else throw new Exception(s"Expected match to `$str` but actually matched `$matchStr`")
        }
      case f : Failure =>
        throw new Exception(f.extra.traced.trace)
    }
  }

  def noParse[T](p: Parser[T], txt: String): Unit = {
    p.parse(txt) match {
      case Success(t, _) =>
        throw new Exception(s"Expecting failure but got $t")
      case f : Failure =>
    }
  }
}
