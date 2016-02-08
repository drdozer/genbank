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
/*
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
        assert(auth == Author(Some("Mathur"), Seq("P"), None))
      }

      * - {
        val txt = "Michelangelo,S.Moerland.M.M., "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Michelangelo"), Seq("S", "Moerland", "M", "M"), None))
      }

      * - {
        val txt = "Vu.J., "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Vu"), Seq("J"), None))
      }

      * - {
        val txt = "Shveta, "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Shveta"), Seq(), None))
      }

      * - {
        val txt = "Shveta, Singh,D.P."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Shveta"), Seq(), None))
      }

      * - {
        val txt = "Kupko,J.J. III,\n            "
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Kupko"), Seq("J", "J"), Some("III")))
      }

      * - {
        val txt = "Sheffield,V, Weber"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Sheffield"), Seq("V"), None))
      }

      * - {
        val txt = "da Silva,W. Jr.,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("da Silva"), Seq("W"), Some("Jr.")))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson, B.T.""".stripMargin
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Clarridge"), Seq("J", "E"), Some("III")))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi."
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Arthi"), Seq("S"), None))
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
        assert(auth == Author(Some("Shobana"), Seq(), None))
      }

      * - {
        val txt = "Sukenik. A.,"
        val auth = parse(GenbankParser.NameWithTrailingSpace, txt)
        assert(auth == Author(Some("Sukenik"), Seq("A"), None))
      }

      * - {
        val txt = "Sukenik. A.,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Sukenik"), Seq("A"), None))
      }

      * - {
        val txt = "S.L. Miller.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Miller"), Seq("S", "L"), None))
      }

      * - {
        val txt = "Hoiby.,N.\n"
        val auth = parse(GenbankParser.WellFormedAuthor, txt)
        assert(auth == Author(Some("Hoiby"), Seq("N"), None))
      }

      * - {
        val txt = "Hoiby.,N.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Hoiby"), Seq("N"), None))
      }

      * - {
        val txt = "Schell,M.,A.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Schell"), Seq("M", "A"), None))
      }

      * - {
        val txt = "Siegele,D,A.,\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Siegele"), Seq("D", "A"), None))
      }

      * - {
        val txt = "Singh,J.A,"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Singh"), Seq("J", "A"), None))
      }

      * - {
        val txt = "St. Leger,R.J.\n"
        val auth = parse(GenbankParser.FamilyName, txt)
        assert(auth == "St. Leger")
      }

      * - {
        val txt = "St. Leger,R.J.\n"
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("St. Leger"), Seq("R", "J"), None))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auth = parse(GenbankParser.Author, txt)
        assert(auth == Author(Some("Clarridge"), Seq("J", "E"), Some("III")))
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
          Author(Some("Screen"), Seq("S", "E"), None),
          Author(Some("Mathur"), Seq("P"), None),
          Author(Some("St. Leger"), Seq("R", "J"), None)
        ), false))
      }

      * - {
        val txt = "Vu.J., Vujicic,M., Church,D.M., "
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths.authors.size == 3)
      }

      * - {
        val txt = "Murray,J., Sheffield,V, Weber,J.L., Duyk,G. and Buetow,K.H.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Murray"), Seq("J"), None),
          Author(Some("Sheffield"), Seq("V"), None),
          Author(Some("Weber"), Seq("J", "L"), None),
          Author(Some("Duyk"), Seq("G"), None),
          Author(Some("Buetow"), Seq("K", "H"), None)
        ), false))
      }

      * - {
        val txt = "Rana,S., Shveta, Singh,D.P.,"
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Rana"), Seq("S"), None),
          Author(Some("Shveta"), Seq(), None),
          Author(Some("Singh"), Seq("D", "P"), None)
        ), false))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Clarridge"), Seq("J", "E"), Some("III"))
        ), false))
      }

      * - {
        val txt = """Clarridge,J.E.
                    |            III and Cookson,B.T.
                    |""".stripMargin
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(
          Author(Some("Clarridge"), Seq("J", "E"), Some("III")),
          Author(Some("Cookson"), Seq("B", "T"), None)
        ), false))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Arthi"), Seq("S"), None))
      }

      * - {
        val txt = "Shobana and Ananthi.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Shobana"), Seq(), None))
      }

      * - {
        val txt = "Ananthi.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Ananthi"), Seq(), None))
      }

      * - {
        val txt = "Arthi,S., Shobana and Ananthi.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Arthi"), Seq("S"), None), Author(Some("Shobana"), Seq(), None), Author(Some("Ananthi"), Seq(), None)), false))
      }

      * - {
        val txt = "Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Gala")
      }

      * - {
        val txt = "Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Gala"), Seq("J", "-L"), None))
      }

      * - {
        val txt = "Philippe,M. and Gala\n            J.-L.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Philippe"), Seq("M"), None), Author(Some("Gala"), Seq("J", "-L"), None)), false))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.FamilyName, txt)
        assert(auths == "Valli Nachiyar")
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.FamilyNameOnly, txt)
        assert(auths == Author(Some("Valli Nachiyar"), Seq(), None))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Valli Nachiyar"), Seq(), None))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C."
        val auths = parse(GenbankParser.AuthorListL, txt)
        assert(auths == AuthorList(Seq(Author(Some("Valli Nachiyar"), Seq(), None)), false))
      }

      * - {
        val txt = "Valli\n            Nachiyar and Rose, C.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Valli Nachiyar"), Seq(), None), Author(Some("Rose"), Seq("C"), None)), false))
      }

      * - {
        val txt = "Frere,J.M. et al.\n"
        val auths = parse(GenbankParser.AuthorList, txt)
        assert(auths == AuthorList(Seq(Author(Some("Frere"), Seq("J", "M"), None)), true))
      }

      * - {
        val txt = "Singh,J.A, Sprott,D. and Tinker,N.A."
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Singh"), Seq("J", "A"), None))
      }

      * - {
        val txt = "Wang,L.H.,,"
        val auths = parse(GenbankParser.Author, txt)
        assert(auths == Author(Some("Wang"), Seq("L", "H"), None))
      }

    }
*/
    'authors {
/*      * - {
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
*/
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
    }
/*
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
    }
*/
  }
  def parse[T](p: Parser[T], txt: String): T = {
    p.parse(txt) match {
      case Success(t, _) =>
        t
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
