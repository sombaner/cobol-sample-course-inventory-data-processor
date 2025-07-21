      IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBEDIT.
      *AUTHOR.        EDWARD SIMLER.
      *INSTALLATION.  CALIFORNIA COMMUNITY COLLEGES.
      *DATE-WRITTEN.  JULY 11, 1989.
       DATE-COMPILED.
      **************************************************************
      *    REMARKS:
      *
      *    THIS PROGRAM WILL PROCESS THE COURSE INVENTORY   DATA FILE
      *    AS SUBMITTED BY THE DISTRICTS.  DURING THE PROCESS ALL DATA
      *    WILL BE EVALUATED TO INSURE THAT THE DATA IS VALID AS
      *    DEFINED BY THE DATA ELEMENT DICTIONARY AND SEVERAL DATA
      *    INTEGRITY CHECKS WILL BE MADE.  ONE   OUTPUT FILE  WILL BE
      *    CREATED.  THIS  FILE  WILL BE FASTLOADED INTO A SUSPENSE FILE
      *    ON THE TERADATA WHERE THE APPROPRIATE REFERENTIAL
      *    INTEGRITY CHECKS WILL BE MADE AS THE DATA IS UPDATED INTO
      *    THE COURSE INVENTORY   DATA TABLE.
      *
      *    LAYOUT OF PROGRAM BY MODULE NUMBERS:
      *
      *      MODULE NUMBER         FUNCTION
      *      -------------         ----------------------------
      *        0000                MAINLINE
      *        1000-1999           INITIALIZE ROUTINES
      *        2000-6999           GENERAL PROCESSING ROUTINES
      *        7000-7999           UTILITY ROUTINES FOR GENERAL
      *                              PROCESSING ROUTINES
      *        8000-8999           ALL INPUT/OUTPUT PROCESSING
      *                              AND OPERATIONS
      *        9000-9999           END OF PROGRAM ROUTINES
      *
      *    INPUTS:
      *        1.  COURSE INVENTORY DATA RECORDS AS SUBMITTED BY THE
      *            DISTRICTS.
      *
      *    OUTPUTS:
      *        1.  COURSE INVENTORY EDITED RECORD FILE.
      *        2.  DETAIL EDIT ERROR REPORT.
      *        3.  SUMMARY EDIT ERROR REPORT.
      *        4.  REPORT TOTALS FILE.
      *
      *    MODIFICATIONS:
      *
      *    1. 2-13-90 COMMENTED OUT INTEGRITY CHECK THAT REQUIRES A
      *       CREDIT COURSE TO HAVE A NON ZERO UNITS MAXIMUM.  E.S.
      *    2. 7-30-90 ADDED OUTPUT DATA SET REPORT-TOTALS-FILE. E.S.
      *    3. 11-1-91  CHANGED PROGRAM FOR PHASE I MODIFICATIONS. KB.
      *       -  RENAMED REMEDIAL-STATUS TO BASIC-SKILLS-STATUS (CB08).
      *          ALLOWED CODE B IN BASIC-SKILLS-STATUS.
      *       -  ALLOWED CODES E,F,Y IN REPEATABILITY.
      *       -  ADDED TWO INTEGRITY CHECKS:
      *          1) IF CB08 = P, THEN CB04 MUST = C.
      *          2) IF CB08 = B, THEN CB04 MUST = C OR N.
      *    4. 3-23-92  CHANGED PROGRAM SO A '0' WILL BE INSERTED INTO
      *       THE LAST POSITION OF THE TOP CODE IF THE FIRST TWO
      *       NUMBERS ARE NOT 49.                            T.N.
      *    5. 7-13-93  (PHASE II) NEW ELEMENTS - PJW
      *       - SPECIAL-CLASS-STATUS
      *       - CAN-CODE, CAN-SEQ-CODE
      *       - SAME-AS-DEPARTMENT-NUMBER1, NUMBER2, NUMBER3
      *       - CROSSWALK-CRS-DEPT-NAME, CROSSWALK-CRS-NUMBER
      *    6. 7-13-93  (PHASE II) CHANGES - PJW
      *       - SAM-PRIORITY-CODE, DROP F,O,X
      *       - COURSE ID CANNOT BE THE SAME AS
      *         SAME-AS-DEPARTMENT-NUMBER1, NUMBER2, OR NUMBER3
      *    7. 8-15-94  (PHASE II) CHANGES - PJW
      *       - SAM-PRIORITY-CODE, NO GROUP 3
      *       - SPECIAL-CLASS-STATUS, NO GROUP 3
      *    8. 1-04-95  KEC
      *       - COMMENTED OUT ALL GROUP 3'S
      *    9. 5-04-95  KEC
      *       - ADDED ELEMENT CB21 (COURSE-PRIOR-TO-COLLEGE-LEVEL)
      *       - ADDED EDIT OF CB03 (TOP CODE) AGAINST NEW TOP5
      *         TABLE.
      *
      ****************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
      *
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURSE-INV-DATA-FILE
                   ASSIGN TO UT-S-CBRECIN.
      *
           SELECT TOPCODE-CHECK-FILE
                   ASSIGN TO UT-S-TOPRECIN.
      *
           SELECT EDITED-COURSE-INV-DATA-FILE
                   ASSIGN TO UT-S-CBRECOUT.
      *
           SELECT REPORT-TOTALS-FILE
                   ASSIGN TO UT-S-TOTSFILE.
      *
           SELECT DATAEDIT-ERROR-REPORT
                   ASSIGN TO UT-S-PRINTER1.
      *
           SELECT SUMMARY-ERROR-REPORT
                   ASSIGN TO UT-S-PRINTER2.
      /
       DATA DIVISION.
       FILE SECTION.
      *
       FD  COURSE-INV-DATA-FILE
                   LABEL RECORDS ARE STANDARD
                   RECORDING MODE IS F
                   BLOCK CONTAINS 0 RECORDS
                   DATA RECORDS ARE COURSE-INVENTORY-RECORD.
      *
       01  COURSE-INVENTORY-RECORD.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *                COURSE INVENTORY INPUT RECORD                  *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  RECORD-CODE                    PIC X(2).
               88  COURSE-DATA-RECORD             VALUE 'CB'.
           03  COLLEGE-ID                     PIC X(3).
           03  TERM-IDENTIFIER.
               05  CALENDAR-YEAR              PIC 9(2).
               05  TERM-CODE                  PIC 9.
           03  COURSE-PERM-DIST-ID            PIC X(12).
           03  COURSE-DEPARTMENT-NUMBER       PIC X(12).
           03  COURSE-TITLE                   PIC X(68).
           03  COURSE-PROGRAM-CODE.
               05  TOP-CODE-FLAG              PIC XX.
               05  TOP-CODE-BODY              PIC XXX.
               05  TOP-CODE-LITERAL           PIC X.
           03  COURSE-CREDIT-STATUS           PIC X(1).
               88  COURSE-CREDIT-STATUS-VALID     VALUE 'D' 'C' 'N'.
               88  CREDIT-STATUS-C                VALUE 'C'.
               88  CREDIT-STATUS-D                VALUE 'D'.
               88  CREDIT-STATUS-N                VALUE 'N'.
               88  CREDIT-STATUS-D-OR-C           VALUE 'D' 'C'.
               88  CREDIT-STATUS-C-OR-N           VALUE 'C' 'N'.
           03  COURSE-TRANSF-STATUS           PIC X(1).
               88  COURSE-TRANSF-STATUS-VALID     VALUE 'A' 'B' 'C'.
               88  TRANSFER-STATUS-A-OR-B         VALUE 'A' 'B'.
               88  TRANSFER-STATUS-C              VALUE 'C'.
           03  COURSE-UNITS-MAXIMUM           PIC X(4).
           03  COURSE-UNITS-MINIMUM           PIC X(4).
           03  COURSE-BASIC-SKILLS-STATUS     PIC X(1).
               88  VALID-BASIC-SKILLS-STATUS      VALUE 'B' 'P' 'N'.
               88  BASIC-SKILLS-STATUS-P          VALUE 'P'.
               88  BASIC-SKILLS-STATUS-B          VALUE 'B'.
           03  COURSE-SAM-PRIORITY-CODE       PIC X(1).
               88  COURSE-SAM-PRIORITY-CODE-VALID VALUE 'A' 'B' 'C'
                                                        'D' 'E'.
           03  COURSE-COOP-ED-STATUS          PIC X(1).
               88  COURSE-COOP-ED-STATUS-VALID    VALUE 'N' 'C' 'G' 'O'.
           03  COURSE-CLASSIFICATION-CODE     PIC X(1).
               88  COURSE-CLASSIFICATION-CD-VALID VALUE 'A' 'B' 'C'
                                                        'D' 'E' 'F'
                                                        'G' 'H' 'I'.
           03  COURSE-REPEATABILITY           PIC X(1).
               88  COURSE-REPEATABILITY-VALID     VALUE 'A' 'B' 'C' 'D'
                                                        'E' 'F' 'Y'.
           03  COURSE-SPECIAL-CLASS-STATUS    PIC X(1).
               88  COURSE-SPECIAL-CLASS-VALID     VALUE 'S' 'N'.
           03  COURSE-CAN-CODE.
               05 COURSE-CAN-CODE-FIRST       PIC X(1).
               05 COURSE-CAN-CODE-REST        PIC X(5).
           03  COURSE-CAN-CODE-R
               REDEFINES COURSE-CAN-CODE      PIC X(6).
               88 COURSE-CAN-CODE-X               VALUE 'XXXXXX'.
               88 COURSE-CAN-CODE-Y               VALUE 'YYYYYY'.
           03  COURSE-CAN-SEQ-CODE.
               05 COURSE-CAN-SEQ-CODE-FIRST   PIC X(1).
               05 COURSE-CAN-SEQ-CODE-REST    PIC X(7).
           03  COURSE-CAN-SEQ-CODE-R
               REDEFINES COURSE-CAN-SEQ-CODE  PIC X(8).
               88 COURSE-CAN-SEQ-CODE-X           VALUE 'XXXXXXXX'.
               88 COURSE-CAN-SEQ-CODE-Y           VALUE 'YYYYYYYY'.
           03  COURSE-SAME-AS-DEPTNO1         PIC X(12).
           03  COURSE-SAME-AS-DEPTNO2         PIC X(12).
           03  COURSE-SAME-AS-DEPTNO3         PIC X(12).
           03  COURSE-CROSSWALK-CRS-NAME.
               05 COURSE-CROSSWALK-NAME-FIRST PIC X(1).
               05 COURSE-CROSSWALK-NAME-REST  PIC X(6).
           03  COURSE-CROSSWALK-CRS-NAME-R
               REDEFINES COURSE-CROSSWALK-CRS-NAME PIC X(7).
               88 COURSE-CRS-NAME-Y                VALUE 'YYYYYYY'.
           03  COURSE-CROSSWALK-CRS-NUMBER.
               05 COURSE-CROSSWALK-NUM-FIRST  PIC X(1).
               05 COURSE-CROSSWALK-NUM-REST   PIC X(8).
           03  COURSE-CROSSWALK-CRS-NUM-R
               REDEFINES COURSE-CROSSWALK-CRS-NUMBER PIC X(9).
               88 COURSE-CRS-NUM-Y                   VALUE 'YYYYYYYYY'.
           03  COURSE-PRIOR-TO-COLLEGE-LEVEL  PIC X.
               88 COURSE-PRIOR-VALID-CODES    VALUE 'A' 'B' 'C' 'Y'.
               88 COURSE-PRIOR-NOT-APP        VALUE 'Y'.
           03  FILLER                         PIC X(31).
      *
      *
       FD  EDITED-COURSE-INV-DATA-FILE
                   LABEL RECORDS ARE STANDARD
                   RECORDING MODE IS F
                   BLOCK CONTAINS 0 RECORDS
                   DATA RECORD IS EDITED-COURSE-INVENTORY-RECORD.
      *
       01  EDITED-COURSE-INVENTORY-RECORD.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *         EDITED COURSE INVENTORY OUTPUT RECORD                 *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  COLLEGE-ID                  PIC X(3).
           03  TERM-IDENTIFIER.
               05  CALENDAR-YEAR           PIC 9(2).
               05  TERM-CODE               PIC 9.
           03  COURSE-PERM-DIST-ID         PIC X(12).
           03  COURSE-DEPARTMENT-NUMBER    PIC X(12).
           03  COURSE-TITLE                PIC X(68).
           03  COURSE-PROGRAM-CODE         PIC X(6).
           03  COURSE-CREDIT-STATUS        PIC X(1).
           03  COURSE-TRANSF-STATUS        PIC X(1).
           03  COURSE-UNITS-MAXIMUM        PIC X(4).
           03  COURSE-UNITS-MINIMUM        PIC X(4).
           03  COURSE-BASIC-SKILLS-STATUS  PIC X(1).
           03  COURSE-SAM-PRIORITY-CODE    PIC X(1).
           03  COURSE-COOP-ED-STATUS       PIC X(1).
           03  COURSE-CLASSIFICATION-CODE  PIC X(1).
           03  COURSE-REPEATABILITY        PIC X(1).
           03  COURSE-SPECIAL-CLASS-STATUS PIC X(1).
           03  COURSE-CAN-CODE             PIC X(6).
           03  COURSE-CAN-SEQ-CODE         PIC X(8).
           03  COURSE-SAME-AS-DEPTNO1      PIC X(12).
           03  COURSE-SAME-AS-DEPTNO2      PIC X(12).
           03  COURSE-SAME-AS-DEPTNO3      PIC X(12).
           03  COURSE-CROSSWALK-CRS-NAME   PIC X(07).
           03  COURSE-CROSSWALK-CRS-NUMBER PIC X(9).
           03  COURSE-PRIOR-TO-COLLEGE-LEVE PIC X.
      *
      *
       FD  REPORT-TOTALS-FILE
                   LABEL RECORDS ARE STANDARD
                   RECORDING MODE IS F
                   BLOCK CONTAINS 0 RECORDS
                   DATA RECORDS ARE REPORT-TOTALS-RECORD1
                                    REPORT-TOTALS-RECORD2
                                    REPORT-TOTALS-RECORD3.
       
       01  REPORT-TOTALS-RECORD1.
           03  TOTALS-ID1.
               05  TOTALS-COLLEGE1     PIC X(3).
               05  TOTALS-TYPE1        PIC X(2).
               05  TOTALS-FLAG1        PIC X(7).
           03  DED-NUMBER              PIC X(4).
           03  EXCEPT-COUNT            PIC 9(6).
           03  UNKNOWN-COUNT           PIC 9(6).
           03  REASON-COUNT            PIC 9(6).
           03  GRP3-COUNT              PIC 9(6).
       
       01  REPORT-TOTALS-RECORD2.
           03  TOTALS-ID2              PIC X(12).
           03  INTEGRITY-ERROR-CODE    PIC X(2).
           03  INTEGRITY-ERROR-COUNT   PIC 9(6).
           03  FILLER                  PIC X(20).
       
       01  REPORT-TOTALS-RECORD3.
           03  TOTALS-ID3.
               05  TOTALS-COLLEGE3     PIC X(3).
               05  TOTALS-TYPE3        PIC X(2).
               05  TOTALS-FLAG3        PIC X(7).
           03  READ-TOTALS             PIC 9(6).
           03  REJECTED-TOTALS         PIC 9(6).
           03  DATE-FLAG               PIC 9(6).
           03  TIME-FLAG               PIC 9(4).
           03  FILLER                  PIC X(6).
      *
      *
       FD  DATAEDIT-ERROR-REPORT
                   LABEL RECORDS ARE OMITTED
                   BLOCK CONTAINS 0 RECORDS
                   RECORDING MODE IS F
                   DATA RECORD IS PRINT-RECORD.
      *
       01  PRINT-RECORD PIC X(133).
      *
       FD  SUMMARY-ERROR-REPORT
                   LABEL RECORDS ARE OMITTED
                   BLOCK CONTAINS 0 RECORDS
                   RECORDING MODE IS F
                   DATA RECORD IS PRINT-RECORD-2.
      *
       01  PRINT-RECORD-2 PIC X(133).
      *
       FD  TOPCODE-CHECK-FILE
                   LABEL RECORDS ARE OMITTED
                   BLOCK CONTAINS 0 RECORDS
                   RECORDING MODE IS F
                   DATA RECORD IS TOPCODE-RECORD.
      *
       01  TOPCODE-RECORD.
           03  TOPCODE                       PIC X(6).
           03  TOP-VOC-ED-FLAG               PIC X.
           03  TOP-TITLE                     PIC X(40).
      *
      /
       WORKING-STORAGE SECTION.
      *
       01  PROGRAM-FLAG-AREA                      VALUE ZEROS.
           03  END-OF-FILE-FLAG              PIC 9.
               88  END-OF-FILE                    VALUE 1.
           03  TOP-EOF-FLAG                  PIC 9.
               88  END-OF-TOP-FILE                VALUE 1.
           03  PAGE-FULL-FLAG                PIC 9.
               88  PAGE-FULL                      VALUE 1.
           03  TABLE-SEARCH-FLAG             PIC 9.
               88  TABLE-SEARCH-COMPLETED         VALUE 1.
           03  ADD-ELEMENT-COLUMNS-FLAG      PIC 9.
               88  ADD-ELEMENT-COLUMNS-DONE       VALUE 1.
           03  NEW-COLLEGE-FLAG              PIC 9.
               88  NEW-COLLEGE                    VALUE 1.
           03  PRINT-LINE-FLAG               PIC 9.
               88  PRINT-LINE-FILLED              VALUE 1.
           03  HEADER-CONTROL-FLAG           PIC 9.
               88  KILL-PROCESS                   VALUE 1.
               88  COLLEGE-ELEMENT-TOTALS         VALUE 2.
               88  COLLEGE-INTEGRITY-TOTALS       VALUE 3.
               88  DISTRICT-ELEMENT-TOTALS        VALUE 4.
               88  DISTRICT-INTEGRITY-TOTALS      VALUE 5.
               88  COLLEGE-SUMMARY-TOTALS         VALUE 6.
               88  DISTRICT-SUMMARY-TOTALS        VALUE 7.
           03  ELEMENT-TABLE-ROW-ID          PIC 99.
               88  ELEMENT-ROWS-1-25              VALUE 1 THRU 25.
               88  ELEMENT-ROWS-1-5               VALUE 1 THRU 5.
               88  ELEMENT-ROWS-20-22             VALUE 20 THRU 22.
               88  ELEMENT-ROWS-13                VALUE 13.
               88  ELEMENT-ROWS-17                VALUE 17.
           03  ELEMENT-TABLE-COLUMN-ID       PIC 9.
               88  ELEMENT-COLUMN-2               VALUE 2.
               88  ELEMENT-COLUMN-3               VALUE 3.
               88  ELEMENT-COLUMN-4               VALUE 4.
           03  COLLEGE-SEARCH-FLAG           PIC 9.
               88  COLLEGE-NOT-FOUND              VALUE 1.
      *
      *
       01  DATA-VALIDATION-FLAGS                  VALUE ZEROS.
           03  REJECT-RECORD-FLAG            PIC 9.
               88  REJECT-RECORD                  VALUE 1.
           03  DISTRICT-ID-FLAG              PIC 9.
               88  DISTRICT-ID-INVALID            VALUE 1.
           03  COLLEGE-ID-FLAG               PIC 9.
               88  COLLEGE-ID-INVALID             VALUE 1.
           03  TERM-ID-FLAG                  PIC 9.
               88  TERM-ID-INVALID                VALUE 1.
           03  COURSE-ID-FLAG                PIC 9.
               88  COURSE-ID-VALID                VALUE 1.
           03  COURSE-TITLE-FLAG             PIC 9.
               88  COURSE-TITLE-VALID             VALUE 1.
           03  COURSE-UNITS-MAXIMUM-FLAG     PIC 9.
               88  COURSE-UNITS-MAXIMUM-VALID     VALUE 1.
           03  COURSE-UNITS-MINIMUM-FLAG     PIC 9.
               88  COURSE-UNITS-MINIMUM-VALID     VALUE 1.
           03  COURSE-PROGRAM-CODE-FLAG      PIC 9.
               88  COURSE-PROGRAM-CODE-VALID      VALUE 1.
           03  TOP-CODE-VALID-FLAG           PIC 9.
               88  TOP-CODE-VALID                 VALUE 1.
           03  COURSE-CAN-CODE-FLAG          PIC 9.
               88  COURSE-CAN-CODE-VALID          VALUE 1.
           03  COURSE-CAN-SEQ-CODE-FLAG      PIC 9.
               88  COURSE-CAN-SEQ-CODE-VALID      VALUE 1.
           03  COURSE-SAME-AS-1-FLAG         PIC 9.
               88  COURSE-SAME-AS-1-VALID         VALUE 1.
           03  COURSE-SAME-AS-2-FLAG         PIC 9.
               88  COURSE-SAME-AS-2-VALID         VALUE 1.
           03  COURSE-SAME-AS-3-FLAG         PIC 9.
               88  COURSE-SAME-AS-3-VALID         VALUE 1.
           03  COURSE-CRS-NAME-FLAG           PIC 9.
               88  COURSE-CRS-NAME-VALID          VALUE 1.
           03  COURSE-CRS-NUMBR-FLAG          PIC 9.
               88  COURSE-CRS-NUMBR-VALID         VALUE 1.
           03  COURSE-PRIOR-LEVEL-FLAG        PIC 9.
               88  COURSE-PRIOR-VALID             VALUE 1.
      *
      *
       01  PROGRAM-LITERAL-AND-WORK.
           03  DETAIL-PROGRAM-NAME     PIC X(13)  VALUE 'CBEDIT'.
           03  SUMMARY-PROGRAM-NAME.
               05  REPORT-CLG-DIST-ID  PIC X(3).
               05  REPORT-TYPE         PIC X(2)   VALUE 'CB'.
               05  REPORT-ID-LIT       PIC X(7).
           03  SUM1-PROGRAM-LIT        PIC X(7)   VALUE 'SYNSUM1'.
           03  SUM2-PROGRAM-LIT        PIC X(7)   VALUE 'SYNSUM2'.
           03  SUM3-PROGRAM-LIT        PIC X(7)   VALUE 'SYNSUM3'.
           03  VALIDITY-LITERAL        PIC X(1)   VALUE 'V'.
           03  INTEGRITY-LITERAL       PIC X(1)   VALUE 'I'.
           03  ONE-LITERAL             PIC 9(1)   VALUE 1.
           03  PRT-PAGE-MAXIMUM        PIC 9(3)   VALUE ZERO.
           03  REJECTED-MESSAGE        PIC X(8)   VALUE 'REJECTED'.
           03  HOLD-COLLEGE-ID         PIC X(3).
           03  HOLD-TIME.
               05  HH-MM               PIC 9(4).
               05  FILLER              PIC 9(4).
           03  TERM-ID-INVALID-LITERAL      PIC X(30)
                       VALUE '  TERM CODE MISSING OR INVALID'.
           03  COLLEGE-INVALID-LITERAL      PIC X(31)
                       VALUE '  COLLEGE ID MISSING OR INVALID'.
           03  DISTRICT-ID-INVALID-LITERAL  PIC X(32)
                       VALUE '  DISTRICT ID MISSING OR INVALID'.
           03  PRT-CNTL-INVALID-LITERAL     PIC X(36)
                       VALUE '  PRINTER CONTROL MISSING OR INVALID'.
           03  ORIGINAL-INDEX-VALUE         USAGE IS INDEX.
           03  NOT-APPLICABLE-LITERAL       PIC X(3)  VALUE 'N/A'.
      *
      *
       01  PROGRAM-ACCUMULATORS                   VALUE ZEROS.
           03  TOTAL-COLLEGE-ENTRIES.
               05  TOTAL-COLLEGE-RECORDS-READ             PIC 9(6).
               05  TOTAL-COLLEGE-RECORDS-WRITTEN          PIC 9(6).
               05  TOTAL-COLLEGE-RECORDS-REJECT           PIC 9(6).
           03  TOTAL-COLLEGE-ENTRY
                       REDEFINES TOTAL-COLLEGE-ENTRIES
                       OCCURS 3 TIMES
                       INDEXED BY TOTAL-COLLEGE-INDEX.
               05  COLLEGE-TOTAL                     PIC 9(6).
           03  COLLEGE-ELEMENT-TOTALS.
               05  COLLEGE-ELEMENT-TOTALS-ROWS
                           OCCURS 25 TIMES
                           INDEXED BY COLLEGE-ELEMENT-ROW-INDEX.
                   07  COLLEGE-ELEMENT-TOTALS-COLUMNS
                               OCCURS 4 TIMES
                               INDEXED BY COLLEGE-ELEMENT-COLUMN-INDEX.
                       09  COLLEGE-ELEMENT-TOTAL    PIC 9(5).
           03  TOTAL-DISTRICT-ENTRIES.
               05  TOTAL-DISTRICT-RECORDS-READ      PIC 9(6).
               05  TOTAL-DISTRICT-RECORDS-WRITTEN   PIC 9(6).
               05  TOTAL-DISTRICT-RECORDS-REJECT    PIC 9(6).
           03  TOTAL-DISTRICT-ENTRY
                       REDEFINES TOTAL-DISTRICT-ENTRIES
                       OCCURS 3 TIMES
                       INDEXED BY TOTAL-DISTRICT-INDEX.
               05  DISTRICT-TOTAL                   PIC 9(6).
           03  DISTRICT-ELEMENT-TOTALS.
               05  DISTRICT-ELEMENT-TOTALS-ROWS
                           OCCURS 25 TIMES
                           INDEXED BY DISTRICT-ELEMENT-ROW-INDEX.
                   07  DISTRICT-ELEMNT-TOTALS-COLUMNS
                               OCCURS 4 TIMES
                               INDEXED BY DISTRICT-ELEMENT-COLUMN-INDEX.
                       09  DISTRICT-ELEMENT-TOTAL   PIC 9(5).
           03  COLLEGE-INTEGRITY-TOTALS.
               05  CLG-INTEGRITY-ERR-1-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-2-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-3-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-4-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-5-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-6-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-7-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-8-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-9-TOTAL        PIC 9(6).
               05  CLG-INTEGRITY-ERR-10-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-11-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-12-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-13-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-14-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-15-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-16-TOTAL       PIC 9(6).
               05  CLG-INTEGRITY-ERR-17-TOTAL       PIC 9(6).
           03  DISTRICT-INTEGRITY-TOTALS.
               05  DIST-INTEGRITY-ERR-1-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-2-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-3-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-4-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-5-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-6-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-7-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-8-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-9-TOTAL       PIC 9(6).
               05  DIST-INTEGRITY-ERR-10-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-11-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-12-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-13-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-14-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-15-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-16-TOTAL      PIC 9(6).
               05  DIST-INTEGRITY-ERR-17-TOTAL      PIC 9(6).
      *
      *
       01  REPORT-TITLE-LINE-1.
           03  FILLER                  PIC X(47)  VALUE SPACE.
           03  FILLER                  PIC X(40)
                VALUE 'COURSE INVENTORY DATA SYNTACTICAL EDIT'.
      *
      *
       01  REPORT-TITLE-LINE-2.
           03  FILLER                  PIC X(50)  VALUE SPACE.
           03  TERM-TITLE              PIC X(19).
           03  FILLER                  PIC X      VALUE SPACE.
           03  FILLER                  PIC X(2)   VALUE '19'.
           03  YEAR-OUT                PIC X(2).
           03  FILLER                  PIC X(3)   VALUE SPACE.
           03  FILLER                  PIC X(1)   VALUE '('.
           03  CALENDAR-YEAR           PIC 99.
           03  FILLER                  PIC X(1)   VALUE '-'.
           03  TERM-CODE               PIC 9.
           03  FILLER                  PIC X(1)   VALUE ')'.
      *
      *
       01  REPORT-TITLE-LINE-3.
           03  FILLER                  PIC X(50)  VALUE SPACE.
           03  COLLEGE-NAME            PIC X(30).
      *
      *
       01  REPORT-TITLE-LINE-4.
           03  FILLER                  PIC X(56)  VALUE SPACE.
           03  FILLER                  PIC X(23)
                       VALUE 'COLLEGE SUMMARY TOTALS:'.
      *
      *
       01  REPORT-TITLE-LINE-5.
           03  FILLER                  PIC X(56)  VALUE SPACE.
           03  FILLER                  PIC X(24)
                       VALUE 'DISTRICT SUMMARY TOTALS:'.
      *
      *
       01  REPORT-TITLE-LINE-6.
           03  FILLER                  PIC X(2).
           03  FILLER                  PIC X(47)  VALUE
                    'PROGRAM ABORTED DUE TO INVALID INPUT PARAMETERS'.
      *
      *
       01  REPORT-TITLE-LINE-7.
           03  FILLER                  PIC X(45)  VALUE SPACE.
           03  FILLER                  PIC X(52)  VALUE
                    'SUMMARY COLLEGE EDIT ERROR TOTALS BY ELEMENT'.
      *
      *
       01  REPORT-TITLE-LINE-8.
           03  FILLER                  PIC X(44)  VALUE SPACE.
           03  FILLER                  PIC X(52)  VALUE
                    'SUMMARY DISTRICT EDIT ERROR TOTALS BY ELEMENT'.
      *
      *
       01  REPORT-TITLE-LINE-9.
           03  FILLER                  PIC X(44)  VALUE SPACE.
           03  FILLER                  PIC X(52)  VALUE
                    'SUMMARY COLLEGE TOTAL INTEGRITY ERRORS BY TYPE'.
      *
      *
       01  REPORT-TITLE-LINE-10.
           03  FILLER                  PIC X(43)  VALUE SPACE.
           03  FILLER                  PIC X(52)  VALUE
                    'SUMMARY DISTRICT TOTAL INTEGRITY ERRORS BY TYPE'.
      *
      *
       01  REPORT-TITLE-LINE-11.
           03  FILLER                  PIC X(61)  VALUE SPACE.
           03  FILLER                  PIC X(52)  VALUE
                    'DETAIL REPORT'.
      *
      *
       01  REPORT-COLUMN-LINE-1.
           03  FILLER                  PIC X.
           03  FILLER                  PIC X(12)  VALUE 'DATA ELEMENT'.
           03  FILLER                  PIC X(5)   VALUE SPACE.
           03  FILLER                  PIC X(4)   VALUE 'DED#'.
      *
      *
       01  ELEMENT-COLUMN-LINE-1.
           03  FILLER                  PIC X(1).
           03  FILLER                  PIC X(48)  VALUE SPACE.
           03  FILLER                  PIC X(5)   VALUE 'TOTAL'.
           03  FILLER                  PIC X(16)  VALUE SPACE.
           03  FILLER                  PIC X(5)   VALUE 'TOTAL'.
           03  FILLER                  PIC X(16)  VALUE SPACE.
           03  FILLER                  PIC X(5)   VALUE 'TOTAL'.
           03  FILLER                  PIC X(16)  VALUE SPACE.
           03  FILLER                  PIC X(5)   VALUE 'TOTAL'.
      *
      *
       01  ELEMENT-COLUMN-LINE-2.
           03  FILLER                  PIC X(1).
           03  FILLER                  PIC X(45)  VALUE SPACE.
           03  FILLER                  PIC X(10)  VALUE '   FIELD  '.
           03  FILLER                  PIC X(12)  VALUE SPACE.
           03  FILLER                  PIC X(9)   VALUE ' UNKNOWN '.
           03  FILLER                  PIC X(9)   VALUE SPACE.
           03  FILLER                  PIC X(14)  VALUE
                   'REASONABLENESS'.
           03  FILLER                  PIC X(11)  VALUE SPACE.
           03  FILLER                  PIC X(7)   VALUE 'GROUP 3'.
      *
      *
       01  ELEMENT-COLUMN-LINE-3.
           03  FILLER                  PIC X(1).
           03  FILLER                  PIC X(16)  VALUE SPACE.
           03  FILLER                  PIC X(21)  VALUE
                   'DATA ELEMENT     DED#'.
           03  FILLER                  PIC X(8)   VALUE SPACE.
           03  FILLER                  PIC X(10)  VALUE 'EXCEPTIONS'.
           03  FILLER                  PIC X(13)  VALUE SPACE.
           03  FILLER                  PIC X(6)   VALUE 'VALUES'.
           03  FILLER                  PIC X(13)  VALUE SPACE.
           03  FILLER                  PIC X(10)  VALUE 'EXECPTIONS'.
           03  FILLER                  PIC X(12)  VALUE SPACE.
           03  FILLER                  PIC X(8)   VALUE '(SPACES)'.
      *
      *
       01  KILL-PROCESS-COLUMN-LINE-1.
           03  FILLER                  PIC X(2).
           03  FILLER                  PIC X(3)   VALUE 'PRT'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'TRM'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'DST'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL1'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL2'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL3'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL4'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL5'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL6'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL7'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL8'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'CL9'.
           03  FILLER                  PIC X(1)   VALUE SPACE.
           03  FILLER                  PIC X(3)   VALUE 'C10'.
      *
      *
       01  EDIT-ERROR-DETAIL-LINE                 VALUE SPACE.
           03  FILLER                  PIC X.
           03  DETAIL-LITERAL-AREA     PIC X(23).
           03  DETAIL-COLUMNS
                       OCCURS 6 TIMES
                       INDEXED BY DETAIL-COLUMN-INDEX.
               05  VALIDATION-FLAG     PIC X.
               05  FILLER              PIC X.
               05  DATA-ELEMENT        PIC X(12).
               05  FILLER              PIC X(4).
      *
      *
       01  ELEMENT-TOTAL-DETAIL-LINE              VALUE SPACE.
           03  FILLER                  PIC X.
           03  FILLER                  PIC X(16).
           03  ELEMENT-LITERAL-AREA    PIC X(26).
           03  FILLER                  PIC X(5).
           03  ELEMENT-COLUMNS
                       OCCURS 4 TIMES
                       INDEXED BY ELEMENT-COLUMN-INDEX.
               05  ELEMENT-TOTAL       PIC ZZ,ZZ9.
               05  ELEMENT-TOTAL-REDEFINES
                       REDEFINES ELEMENT-TOTAL.
                   07  FILLER          PIC X(3).
                   07  ELEMENT-N-A     PIC X(3).
               05  FILLER              PIC X(15).
      *
      *
       01  TOTAL-DESCRIPTIONS.
           03  TOTAL-DESCRIPTION-ENTRIES.
               05  FILLER              PIC X(30)
                           VALUE 'RECORDS READ:'.
               05  FILLER              PIC X(30)
                           VALUE 'RECORDS WRITTEN:'.
               05  FILLER              PIC X(30)
                           VALUE 'RECORDS REJECTED:'.
           03  TOTAL-DESCRIPTION-ENTRY
                   REDEFINES TOTAL-DESCRIPTION-ENTRIES
                   OCCURS 3 TIMES
                   INDEXED BY TOTAL-DESCRIPTION-INDEX.
               05  TOTAL-DESCRIPTION  PIC X(30).
      *
      *
       01  TOTAL-DETAIL-LINE                      VALUE SPACE.
           03  FILLER                  PIC X(48).
           03  TOTAL-DESCRIPTION       PIC X(29).
           03  TOTAL-FIELD             PIC ZZZ,ZZ9.
      *
      *
       01  TOP-CODE-TABLE                       VALUE '999999'.
           03  TOP-CODE-TBL
                       OCCURS 350 TIMES
                       ASCENDING KEY IS TOP-CODE-Y
                       INDEXED BY TOPIDX.
               05  TOP-CODE-Y                   PIC X(6).
      *
      *
       01  INTEGRITY-DESCRIPTIONS.
           03  INTEGRITY-ERR-1-DESC.
               05  INT-ERR-1-PART-1   PIC X(35)
                   VALUE 'COURSE TRANSFERABLE (CB05) BUT NOT '.
               05  INT-ERR-1-PART-2   PIC X(31)
                   VALUE 'CREDIT-DEGREE APPLICABLE (CB04)'.
           03  INTEGRITY-ERR-2-DESC.
               05  INT-ERR-2-PART-1   PIC X(38)
                   VALUE 'COURSE IS NON-CREDIT (CB04) BUT UNITS '.
               05  INT-ERR-2-PART-2   PIC X(40)
                   VALUE 'MAXIMUM (CB06) WERE REPORTED AS NON-ZERO'.
           03  INTEGRITY-ERR-3-DESC.
               05  INT-ERR-3-PART-1   PIC X(38)
                   VALUE 'COURSE IS NON-CREDIT (CB04) BUT UNITS '.
               05  INT-ERR-3-PART-2   PIC X(40)
                   VALUE 'MINIMUM (CB07) WERE REPORTED AS NON-ZERO'.
           03  INTEGRITY-ERR-4-DESC.
               05  INT-ERR-4-PART-1   PIC X(38)
                   VALUE 'COURSE BASIC SKILLS STATUS (CB08) = P '.
               05  INT-ERR-4-PART-2   PIC X(36)
                   VALUE 'BUT CREDIT STATUS (CB04) DID NOT = C'.
           03  INTEGRITY-ERR-5-DESC.
               05  INT-ERR-5-PART-1   PIC X(38)
                   VALUE 'COURSE BASIC SKILLS STATUS (CB08) = B '.
               05  INT-ERR-5-PART-2   PIC X(41)
                   VALUE 'BUT CREDIT STATUS (CB04) DID NOT = C OR N'.
           03  INTEGRITY-ERR-6-DESC.
               05  INT-ERR-6-PART-1   PIC X(37)
                   VALUE 'WHEN CAN CODE (CB14) IS CODED WITH X '.
               05  INT-ERR-6-PART-2   PIC X(35)
                   VALUE 'CAN SEQ CODE (CB15) CANNOT = X OR Y'.
           03  INTEGRITY-ERR-7-DESC.
               05  INT-ERR-7-PART-1   PIC X(37)
                   VALUE 'WHEN CAN CODE (CB14) IS CODED WITH Y '.
               05  INT-ERR-7-PART-2   PIC X(33)
                   VALUE 'CAN SEQ CODE (CB15) MUST ALSO = Y'.
           03  INTEGRITY-ERR-8-DESC.
               05  INT-ERR-8-PART-1   PIC X(42)
                   VALUE 'WHEN CAN CODE (CB14) IS CODED WITH A CODE '.
               05  INT-ERR-8-PART-2   PIC X(34)
                   VALUE 'THE CAN SEQ CODE (CB15) CANNOT = Y'.
           03  INTEGRITY-ERR-9-DESC.
               05  INT-ERR-9-PART-1   PIC X(40)
                   VALUE 'SAME AS DEPARTMENT NO 1 (CB16) CANNOT = '.
               05  INT-ERR-9-PART-2   PIC X(39)
                   VALUE 'THE COURSE ID (CB01) IN THE SAME RECORD'.
           03  INTEGRITY-ERR-10-DESC.
               05  INT-ERR-10-PART-1   PIC X(40)
                   VALUE 'SAME AS DEPARTMENT NO 2 (CB17) CANNOT = '.
               05  INT-ERR-10-PART-2   PIC X(39)
                   VALUE 'THE COURSE ID (CB01) IN THE SAME RECORD'.
           03  INTEGRITY-ERR-11-DESC.
               05  INT-ERR-11-PART-1   PIC X(40)
                   VALUE 'SAME AS DEPARTMENT NO 3 (CB18) CANNOT = '.
               05  INT-ERR-11-PART-2   PIC X(39)
                   VALUE 'THE COURSE ID (CB01) IN THE SAME RECORD'.
           03  INTEGRITY-ERR-12-DESC.
               05  INT-ERR-12-PART-1   PIC X(40)
                   VALUE 'WHEN TRANSFER STATUS (CB05) IS = A OR B '.
               05  INT-ERR-12-PART-2   PIC X(31)
                   VALUE 'THEN CRS NAME (CB19) CANNOT = Y'.
           03  INTEGRITY-ERR-13-DESC.
               05  INT-ERR-13-PART-1   PIC X(35)
                   VALUE 'WHEN TRANSFER STATUS (CB05) IS = C '.
               05  INT-ERR-13-PART-2   PIC X(32)
                   VALUE 'THEN CRS NAME (CB19) MUST BE = Y'.
           03  INTEGRITY-ERR-14-DESC.
               05  INT-ERR-14-PART-1   PIC X(40)
                   VALUE 'WHEN TRANSFER STATUS (CB05) IS = A OR B '.
               05  INT-ERR-14-PART-2   PIC X(31)
                   VALUE 'THEN CRS NMBR (CB20) CANNOT = Y'.
           03  INTEGRITY-ERR-15-DESC.
               05  INT-ERR-15-PART-1   PIC X(35)
                   VALUE 'WHEN TRANSFER STATUS (CB05) IS = C '.
               05  INT-ERR-15-PART-2   PIC X(32)
                   VALUE 'THEN CRS NMBR (CB20) MUST BE = Y'.
           03  INTEGRITY-ERR-16-DESC.
               05  INT-ERR-16-PART-1   PIC X(41)
                   VALUE 'WHEN CRS PRIOR TO COLL LVL (CB21) = A, B,'.
               05  INT-ERR-16-PART-2   PIC X(42)
                   VALUE ' C, THEN CRS TRNSF STAT (CB05) MUST BE = C'.
      *    03  INTEGRITY-ERR-17-DESC.
      *        05  INT-ERR-17-PART-1   PIC X(26)
      *            VALUE 'CRSE PGM CODE (CB03) NOT ='.
      *        05  INT-ERR-17-PART-2   PIC X(33)
      *            VALUE ' TO ANY CODE IN TOP5 LOOKUP TABLE'.
      *
      *
       01  INTEGRITY-TOTAL-DETAIL-LINE            VALUE SPACE.
           03  FILLER                  PIC X(01).
           03  INTEGRITY-TOTAL         PIC ZZZ,ZZ9.
           03  FILLER                  PIC X(2).
           03  INTEGRITY-DESCRIPTION   PIC X(80).
      *
      *
       01  BLANK-LINE.
           03  FILLER                  PIC X(133) VALUE SPACE.
      *
      *
       01  RUNTIME-PARAMETER-TABLE.
           03  RUNTIME-PARAMETER-COLLEGES
                       OCCURS 10 TIMES
                       INDEXED BY RUNTIME-PARAMETER-INDEX.
               05  COLLEGE-ID          PIC X(3).
               05  COLLEGE-NAME        PIC X(30).
      *
      *
       01  EDIT-ERROR-LITERAL-TABLE.
           03  EDIT-ERROR-LITERAL-ENTRIES.
               05  FILLER              PIC X(17)
                       VALUE 'PERMANENT DST ID'.
               05  FILLER              PIC X(6)   VALUE 'CB00'.
               05  FILLER              PIC X(17)
                       VALUE 'DEPARTMENT NUM.'.
               05  FILLER              PIC X(6)   VALUE 'CB01'.
               05  FILLER              PIC X(17)  VALUE 'RECORD CODE'.
               05  FILLER              PIC X(6)   VALUE 'GI90'.
               05  FILLER              PIC X(17)  VALUE 'COLLEGE ID'.
               05  FILLER              PIC X(6)   VALUE 'GI01'.
               05  FILLER              PIC X(17)  VALUE 'TERM ID'.
               05  FILLER              PIC X(6)   VALUE 'GI03'.
               05  FILLER              PIC X(17)  VALUE 'TITLE'.
               05  FILLER              PIC X(6)   VALUE 'CB02'.
               05  FILLER              PIC X(17)
                       VALUE 'PROGRAM CODE'.
               05  FILLER              PIC X(6)   VALUE 'CB03'.
               05  FILLER              PIC X(17)
                       VALUE 'CREDIT STATUS'.
               05  FILLER              PIC X(6)   VALUE 'CB04'.
               05  FILLER              PIC X(17)
                       VALUE 'TRANSFER STATUS'.
               05  FILLER              PIC X(6)   VALUE 'CB05'.
               05  FILLER              PIC X(17)
                       VALUE 'UNITS CR MAXIMUM'.
               05  FILLER              PIC X(6)   VALUE 'CB06'.
               05  FILLER              PIC X(17)
                       VALUE 'UNITS CR MINIMUM'.
               05  FILLER              PIC X(6)   VALUE 'CB07'.
               05  FILLER              PIC X(17)
                       VALUE 'BASC SKILLS STAT'.
               05  FILLER              PIC X(6)   VALUE 'CB08'.
               05  FILLER              PIC X(17)
                       VALUE 'SAM PRIORITY CD'.
               05  FILLER              PIC X(6)   VALUE 'CB09'.
               05  FILLER              PIC X(17)
                       VALUE 'COOP ED STATUS'.
               05  FILLER              PIC X(6)   VALUE 'CB10'.
               05  FILLER              PIC X(17)
                       VALUE 'CLASSIFICAT CODE'.
               05  FILLER              PIC X(6)   VALUE 'CB11'.
               05  FILLER              PIC X(17)
                       VALUE 'REPEATABILITY'.
               05  FILLER              PIC X(6)   VALUE 'CB12'.
               05  FILLER              PIC X(17)
                       VALUE 'SPECIAL CLASS ST'.
               05  FILLER              PIC X(6)   VALUE 'CB13'.
               05  FILLER              PIC X(17)
                       VALUE 'CAN CODE'.
               05  FILLER              PIC X(6)   VALUE 'CB14'.
               05  FILLER              PIC X(17)
                       VALUE 'CAN SEQ CODE'.
               05  FILLER              PIC X(6)   VALUE 'CB15'.
               05  FILLER              PIC X(17)
                       VALUE 'SAME AS DEPT NUM1'.
               05  FILLER              PIC X(6)   VALUE 'CB16'.
               05  FILLER              PIC X(17)
                       VALUE 'SAME AS DEPT NUM2'.
               05  FILLER              PIC X(6)   VALUE 'CB17'.
               05  FILLER              PIC X(17)
                       VALUE 'SAME AS DEPT NUM3'.
               05  FILLER              PIC X(6)   VALUE 'CB18'.
               05  FILLER              PIC X(17)
                       VALUE 'CRS NAME'.
               05  FILLER              PIC X(6)   VALUE 'CB19'.
               05  FILLER              PIC X(17)
                       VALUE 'CRS NUM'.
               05  FILLER              PIC X(6)   VALUE 'CB20'.
               05  FILLER              PIC X(17)
                       VALUE 'CRS PRIOR LVL'.
               05  FILLER              PIC X(6)   VALUE 'CB21'.
               05  FILLER              PIC X(17)  VALUE '  ACTION:'.
               05  FILLER              PIC X(6).
               05  FILLER              PIC X(17).
               05  FILLER              PIC X(6).
           03  EDIT-ERROR-LITERAL-ENTRY
                       REDEFINES EDIT-ERROR-LITERAL-ENTRIES
                       OCCURS 27 TIMES
                       INDEXED BY EDIT-ERROR-LITERAL-INDEX.
               05  EDIT-ERROR-LITERAL-AREA.
                   07  EDIT-ERROR-LITERAL-1    PIC X(17).
                   07  EDIT-ERROR-LITERAL-2    PIC X(06).
      *
      *
       01  EDIT-ERROR-TABLE.
           03  EDIT-ERROR-TABLE-ROWS
                       OCCURS 27 TIMES
                       INDEXED BY EDIT-ERROR-ROW-INDEX.
               05  EDIT-ERROR-TABLE-COLUMNS
                           OCCURS 6 TIMES
                           INDEXED BY EDIT-ERROR-COLUMN-INDEX.
                   07  DATA-ELEMENT            PIC X(12).
                   07  VALIDATION-FLAG         PIC X(01).
      *
      *
       01  TERM-IDENTIFIER-TABLE.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *                   TERM IDENTIFIER TABLE                       *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  TERM-IDENTIFIER-ENTRIES.
               05  ANNUAL-TERM.
                   07  TERM-CODE       PIC X(1)   VALUE '0'.
                   07  TERM-TITLE      PIC X(19)  VALUE 'ANNUAL'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 10.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 15.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  WINTER-INTERCESSION.
                   07  TERM-CODE       PIC X(1)   VALUE '1'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'WINTER INTERCESSION'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 02.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  WINTER-QUARTER.
                   07  TERM-CODE       PIC X(1)   VALUE '2'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'WINTER QUARTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 02.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE 1.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  SPRING-SEMESTER.
                   07  TERM-CODE       PIC X(1)   VALUE '3'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'SPRING SEMESTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 03.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  SPRING-QUARTER.
                   07  TERM-CODE       PIC X(1)    VALUE '4'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'SPRING QUARTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 03.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE 1.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  SUMMER-TERM.
                   07  TERM-CODE       PIC X(1)   VALUE '5'.
                   07  TERM-TITLE      PIC X(19)  VALUE 'SUMMER TERM'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 07.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  SUMMER-QUARTER.
                   07  TERM-CODE       PIC X(1)   VALUE '6'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'SUMMER QUARTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 07.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 01.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE 1.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  FALL-SEMESTER.
                   07  TERM-CODE       PIC X(1)   VALUE '7'.
                   07  TERM-TITLE      PIC X(19)
                               VALUE 'FALL SEMESTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 10.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 15.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  FALL-QUARTER.
                   07  TERM-CODE       PIC X(1)   VALUE '8'.
                   07  TERM-TITLE      PIC X(19)  VALUE 'FALL QUARTER'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 10.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 15.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE 1.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
               05  NONE-ABOVE.
                   07  TERM-CODE       PIC X(1)   VALUE '9'.
                   07  TERM-TITLE      PIC X(19)  VALUE 'UNKNOWN'.
                   07  BIRTH-COMPUTATION-DATE.
                       09  AGE-COMPUTATION-MONTH
                                       PIC 9(2)   VALUE 10.
                       09  AGE-COMPUTATION-DAY
                                       PIC 9(2)   VALUE 15.
                   07  QUARTER-TYPE-TERM-FLAG
                                       PIC 9      VALUE ZERO.
                   07  EFFECTIVE-DATES.
                       09  BEGIN-DATE  PIC 9(6)   VALUE ZEROES.
                       09  END-DATE    PIC 9(6)   VALUE 999999.
           03  TERM-IDENTIFIER-ENTRY
                       REDEFINES TERM-IDENTIFIER-ENTRIES
                       OCCURS 10 TIMES
                       INDEXED BY TERM-IDENTIFIER-INDEX.
               05  TERM-CODE         PIC X(1).
               05  TERM-TITLE        PIC X(19).
               05  BIRTH-COMPUTATION-DATE.
                   07  AGE-COMPUTATION-MONTH
                                     PIC 9(2).
                   07  AGE-COMPUTATION-DAY
                                     PIC 9(2).
               05  QUARTER-TYPE-TERM-FLAG
                                     PIC 9.
                   88  TERM-IS-A-QUARTER         VALUE 1.
               05  EFFECTIVE-DATES.
                   07  BEGIN-DATE      PIC 9(6).
                   07  END-DATE        PIC 9(6).
      *
      *
       01  DISTRICT-CODES-TABLE.
      *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                             *
      *            D I S T R I C T   C O D E   T A B L E            *
      *                                                             *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  DISTRICT-CODE-VALUES.
               05  ALLAN-HANCOCK.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69096.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 610.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'ALLAN HANCOCK DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9001A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  ANTELOPE-VALLEY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64253.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 620.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'ANTELOPE VALLEY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9002A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  BARSTOW.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67629.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 910.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'BARSTOW DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9003A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  BUTTE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61416.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 110.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'BUTTE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9004A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  CABRILLO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69740.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 410.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CABRILLO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9005A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  CERRITOS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64360.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 810.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CERRITOS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9006A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  CHAFFEY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67660.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 920.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CHAFFEY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9007A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  CITRUS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64386.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 820.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CITRUS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9008A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  DESERT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67025.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 930.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'DESERT DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9009A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  COAST-DISTRICT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66639.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 830.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'COAST DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9010A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  COMPTON.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64428.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 710.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'COMPTON DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9011A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  CONTRA-COSTA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61689.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 310.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CONTRA COSTA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9012A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  EL-CAMINO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64493.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 720.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'EL CAMINO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9013A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  FEATHER-RIVER.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 75143.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 120.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'FEATHER RIVER DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9071A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  FOOTHILL-DISTRICT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69443.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 420.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'FOOTHILL DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9014A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  FREMONT-NEWARK.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61184.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 430.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'FREMONT-NEWARK DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9015A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  GAVILAN.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69476.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 440.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'GAVILAN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9016A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  GLENDALE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73486.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 730.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'GLENDALE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9017A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  GROSSMONT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68148.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 020.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'GROSSMONT DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9018A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  HARTNELL.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66043.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 450.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'HARTNELL DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9019A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  IMPERIAL.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63156.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 030.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'IMPERIAL DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9020A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  KERN-DISTRICT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63537.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 520.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'KERN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9021A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  LAKE-TAHOE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73775.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 220.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'LAKE TAHOE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9022A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  LASSEN.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64147.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 130.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'LASSEN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9023A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  LONG-BEACH.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73494.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 840.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'LONG BEACH DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9024A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  LOS-ANGELES.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 740.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'LOS ANGELES DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9025A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  LOS-RIOS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67371.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 230.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'LOS RIOS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9026A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  MARIN-DISTRICT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65383.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 330.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MARIN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9027A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  MENDOCINO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73718.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 140.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MENDOCINO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9028A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  MERCED.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65797.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 530.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MERCED DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9029A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  MIRA-COSTA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68247.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 050.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MIRA COSTA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9030A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  MONTEREY-PENINSULA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66100.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 460.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MONTEREY PENINSULA DIST'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9031A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  MT-SAN-ANTONIO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64824.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 850.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MT. SAN ANTONIO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9032A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  MT-SAN-JACINTO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67132.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 940.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'MT. SAN JACINTO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9033A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  NAPA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66274.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 240.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'NAPA VALLEY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9034A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  NORTH-ORANGE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66605.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 860.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'NORTH ORANGE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9035A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  PALOMAR.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68270.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 060.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'PALOMAR DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9037A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  PALO-VERDE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73510.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 950.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'PALO VERDE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9036A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  PASADENA-AREA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64899.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 770.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'PASADENA AREA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9038A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  PERALTA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61267.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 340.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'PERALTA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9039A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  RANCHO-SANTIAGO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66688.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 870.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'RANCHO SANTIAGO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9040A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  REDWOODS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62992.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 160.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'REDWOODS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9041A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  RIO-HONDO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64923.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 880.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'RIO HONDO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9042A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  RIVERSIDE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67223.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 960.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'RIVERSIDE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9043A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SADDLEBACK.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66654.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 890.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SADDLEBACK DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9044A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SAN-BERNARDINO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67884.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 980.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN BERNARDINO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9045A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SAN-DIEGO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 070.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN DIEGO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9046A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SAN-FRANCISCO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73536.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 360.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN FRANCISCO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9047A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SAN-JOAQUIN-DELTA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68668.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 550.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN JOAQUIN DELTA DIST'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9048A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SAN-JOSE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69658.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 470.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN JOSE/EVERGREEN DIST'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9049A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SAN-LUIS-OBISPO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68817.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 640.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN LUIS OBISPO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9050A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SAN-MATEO.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69054.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 370.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SAN MATEO DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9051A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SANTA-BARBARA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69294.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 650.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SANTA BARBARA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9052A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SANTA-CLARITA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64972.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 660.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SANTA CLARITA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9053A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SANTA-MONICA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73502.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 780.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SANTA MONICA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9054A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SEQUOIAS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72124.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 560.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SEQUOIAS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9055A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SHASTA-TE-TR.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70144.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 170.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SHASTA-TE-TR DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9056A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SIERRA.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66936.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 270.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SIERRA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9057A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SISKIYOU.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70474.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 180.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SISKIYOU DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9058A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SOLAN0-COUNTY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70557.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 280.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SOLANO COUNTY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9059A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SONOMA-COUNTY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70946.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 260.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SONOMA COUNTY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9060A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  SOUTH-COUNTY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61317.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 480.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'CHABOT-LAS POSITAS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9061A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  STATE-CENTER.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62463.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 570.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'STATE CENTER DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9062A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  SOUTHWESTERN.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68429.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 090.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'SOUTHWESTERN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9063A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  VENTURA-COUNTY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72660.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 680.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'VENTURA COUNTY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9064A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  VICTOR-VALLEY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67926.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 990.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'VICTOR VALLEY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9065A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  WEST-HILLS.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62133.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 580.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'WEST HILLS DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9066A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  WEST-KERN.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63867.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 690.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'WEST KERN DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9067A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
               05  WEST-VALLEY.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69716.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 490.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'WEST VALLEY DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9068A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  YOSEMITE.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 71340.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 590.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'YOSEMITE DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9069A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE ZERO.
               05  YUBA-DISTRICT.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72777.
                   07  DISTRICT-ID     PIC 9(3)   VALUE 290.
                   07  DISTRICT-NAME   PIC X(30)
                           VALUE 'YUBA DISTRICT'.
                   07  CPEC-DISTRICT-CODE
                                       PIC X(6)   VALUE 'C9070A'.
                   07  SINGLE-COLLEGE-FLAG
                                       PIC 9      VALUE 1.
           03  DISTRICT-CODE-ENTRY
                       REDEFINES DISTRICT-CODE-VALUES
                       OCCURS 71 TIMES
                       INDEXED BY DISTRICT-CODE-INDEX.
               05  DISTRICT-CODE       PIC 9(5).
               05  DISTRICT-ID         PIC 9(3).
               05  DISTRICT-NAME       PIC X(30).
               05  CPEC-DISTRICT-CODE  PIC X(6).
               05  SINGLE-COLLEGE-FLAG PIC 9.
                   88  SINGLE-COLLEGE-DISTRICT    VALUE 1.
      *
      *
      *
       01  COLLEGE-CODES-TABLE.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                           *
      *        C O L L E G E   C O D E  T A B L E                 *
      *                                                           *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  COLLEGE-CODE-VALUES.
               05  LAS-POSITAS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '481'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61317.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0013A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LAS POSITAS COLLEGE'.
               05  CHABOT-HAYWARD.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015235.
                   07  COLLEGE-ID      PIC X(3)   VALUE '482'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61317.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0013A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001162.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CHABOT COLLEGE'.
               05  VISTA-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015236.
                   07  COLLEGE-ID      PIC X(3)   VALUE '345'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61267.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0069A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'VISTA COLLEGE'.
               05  ALAMEDA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015257.
                   07  COLLEGE-ID      PIC X(3)   VALUE '341'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61267.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0001A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008306.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'ALAMEDA, COLLEGE OF'.
               05  LANEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015450.
                   07  COLLEGE-ID      PIC X(3)   VALUE '343'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61267.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0041A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001266.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LANEY COLLEGE'.
               05  MERRITT.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015570.
                   07  COLLEGE-ID      PIC X(3)   VALUE '344'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61267.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0055A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001267.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MERRITT COLLEGE'.
               05  OHLONE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 015610.
                   07  COLLEGE-ID      PIC X(3)   VALUE '431'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61184.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0063A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 004481.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'OHLONE COLLEGE'.
               05  BUTTE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 045115.
                   07  COLLEGE-ID      PIC X(3)   VALUE '111'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61416.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0007A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008073.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'BUTTE COLLEGE'.
               05  CONTRA-COSTA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 075190.
                   07  COLLEGE-ID      PIC X(3)   VALUE '311'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61689.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0018A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001190.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CONTRA COSTA COLLEGE'.
               05  DIABLO-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 075268.
                   07  COLLEGE-ID      PIC X(3)   VALUE '312'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61689.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0025A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001191.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'DIABLO VALLEY COLLEGE'.
               05  LOS-MEDANOS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 075269.
                   07  COLLEGE-ID      PIC X(3)   VALUE '313'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 61689.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0051A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 010340.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS MEDANOS COLLEGE'.
               05  LAKE-TAHOE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 095001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '221'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73775.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0040A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 012907.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LAKE TAHOE COMMUNITY COLLEGE'.
               05  WEST-HILLS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 105131.
                   07  COLLEGE-ID      PIC X(3)   VALUE '581'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62133.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0100A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001176.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'WEST HILLS COLLEGE'.
               05  FRESNO-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 105240.
                   07  COLLEGE-ID      PIC X(3)   VALUE '571'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62463.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0031A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001307.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'FRESNO CITY COLLEGE'.
               05  KINGS-RIVER.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 105523.
                   07  COLLEGE-ID      PIC X(3)   VALUE '572'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62463.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0072A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001308.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'KINGS RIVER COMMUNITY COLLEGE'.
               05  REDWOODS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 125140.
                   07  COLLEGE-ID      PIC X(3)   VALUE '161'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 62992.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0071A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001185.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'REDWOODS, COLLEGE OF THE'.
               05  IMPERIAL-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 135570.
                   07  COLLEGE-ID      PIC X(3)   VALUE '031'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63156.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0038A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001214.
                   07 COLLEGE-NAME     PIC X(30)
                           VALUE 'IMPERIAL VALLEY COLLEGE'.
               05  BAKERSFIELD.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 155050.
                   07  COLLEGE-ID      PIC X(3)   VALUE '521'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63537.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0005A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001118.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'BAKERSFIELD COLLEGE'.
               05  CERRO-COSO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 155001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '522'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63537.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0012A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 010111.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CERRO COSO COMMUNITY COLLEGE'.
               05  PORTERVILLE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 545364.
                   07  COLLEGE-ID      PIC X(3)   VALUE '523'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63537.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0070A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001268.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'PORTERVILLE COLLEGE'.
               05  TAFT-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 155580.
                   07  COLLEGE-ID      PIC X(3)   VALUE '691'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 63867.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0097A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001309.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'TAFT COLLEGE'.
               05  LASSEN-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 185420.
                   07  COLLEGE-ID      PIC X(3)   VALUE '131'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64147.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0042A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001217.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LASSEN COLLEGE'.
               05  LOS-ANGELES-ITV.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195000.
                   07  COLLEGE-ID      PIC X(3)   VALUE '74A'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES ITV'.
               05  ANTELOPE-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195020.
                   07  COLLEGE-ID      PIC X(3)   VALUE '621'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64253.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0004A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001113.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'ANTELOPE VALLEY COLLEGE'.
               05  CERRITOS-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195154.
                   07  COLLEGE-ID      PIC X(3)   VALUE '811'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64360.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0011A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001161.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CERRITOS COLLEGE'.
               05  COLLEGE-OF-CANYONS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195175.
                   07  COLLEGE-ID      PIC X(3)   VALUE '661'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64972.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0010A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008903.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CANYONS, COLLEGE OF THE'.
               05  CITRUS-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195177.
                   07  COLLEGE-ID      PIC X(3)   VALUE '821'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64386.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0015A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001166.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CITRUS COLLEGE'.
               05  COMPTON-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195196.
                   07  COLLEGE-ID      PIC X(3)   VALUE '711'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64428.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0017A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001188.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'COMPTON COMMUNITY COLLEGE'.
               05  EAST-LOS-ANGELES.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195217.
                   07  COLLEGE-ID      PIC X(3)   VALUE '748'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0026A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001222.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'EAST LOS ANGELES COLLEGE'.
               05  EL-CAMINO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195225.
                   07  COLLEGE-ID      PIC X(3)   VALUE '721'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64493.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0027A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001197.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'EL CAMINO COLLEGE'.
               05  GLENDALE-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195257.
                   07  COLLEGE-ID      PIC X(3)   VALUE '731'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73486.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0034A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 01203.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'GLENDALE COLLEGE'.
               05  LONG-BEACH.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195337.
                   07  COLLEGE-ID      PIC X(3)   VALUE '841'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73494.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0043A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001219.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LONG BEACH CITY COLLEGE'.
               05  LOS-ANGELES-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195346.
                   07  COLLEGE-ID      PIC X(3)   VALUE '741'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0044A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 01223.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES CITY COLLEGE'.
               05  LOS-ANGELES-HARBOR.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195365.
                   07  COLLEGE-ID      PIC X(3)   VALUE '742'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0045A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001224.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES HARBOR COLLEGE'.
               05  LOS-ANGELES-PIERCE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195384.
                   07  COLLEGE-ID      PIC X(3)   VALUE '744'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0047A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001226.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES PIERCE COLLEGE'.
               05  LOS-ANGELES-SOUTHWEST.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195387.
                   07  COLLEGE-ID      PIC X(3)   VALUE '745'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0048A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007047.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES SOUTHWEST COLLEGE'.
               05  LOS-ANGELES-TRADE-TECH.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195390.
                   07  COLLEGE-ID      PIC X(3)   VALUE '746'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CEPC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0049A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001227.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES TRADE-TECHNICAL CO'.
               05  LOS-ANGELES-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195396.
                   07  COLLEGE-ID      PIC X(3)   VALUE '747'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0050A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001228.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES VALLEY COLLEGE'.
               05  MT-SAN-ANTONIO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195475.
                   07  COLLEGE-ID      PIC X(3)   VALUE '851'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64824.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0060A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001245.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MOUNT SAN ANTONIO COLLEGE'.
               05  PASADENA-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195575.
                   07  COLLEGE-ID      PIC X(3)   VALUE '771'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64899.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0068A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001261.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'PASADENA CITY COLLEGE'.
               05  RIO-HONDO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195658.
                   07  COLLEGE-ID      PIC X(3)   VALUE '881'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64923.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0073A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001269.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'RIO HONDO COLLEGE'.
               05  SANTA-MONICA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195825.
                   07  COLLEGE-ID      PIC X(3)   VALUE '781'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73502.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0088A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001286.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SANTA MONICA CITY COLLEGE'.
               05  WEST-LOS-ANGELES.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195952.
                   07  COLLEGE-ID      PIC X(3)   VALUE '749'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0101A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008596.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'WEST LOS ANGELES COLLEGE'.
               05  L-A-MISSION.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 195953.
                   07  COLLEGE-ID      PIC X(3)   VALUE '743'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 64741.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0046A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 012550.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'LOS ANGELES MISSION COLLEGE'.
               05  INDIAN-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 215001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '331'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65383.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0039A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 011730.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'INDIAN VALLEY COLLEGE'.
               05  INDIAN-VALLEY-NC.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 215002.
                   07  COLLEGE-ID      PIC X(3)   VALUE '332'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65383.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0039A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'INDIAN VALLEY CONTINUING EDUCA'.
               05  MARIN-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 215060.
                   07  COLLEGE-ID      PIC X(3)   VALUE '334'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65383.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0052A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001178.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MARIN, COLLEGE OF'.
               05  MARIN-NC.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 215061.
                   07  COLLEGE-ID      PIC X(3)   VALUE '335'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65383.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MARIN CONTINUING EDUCATION CEN'.
               05  MENDOCINO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 235001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '141'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73718.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0053A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 011672.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MENDOCINO COLLEGE'.
               05  MERCED.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 245475.
                   07  COLLEGE-ID      PIC X(3)   VALUE '531'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 65797.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0054A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001237.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MERCED COLLEGE'.
               05  HARTNELL.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 275129.
                   07  COLLEGE-ID      PIC X(3)   VALUE '451'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66043.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0037A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001209.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'HARTNELL COLLEGE'.
               05  MONTEREY-PENINSULA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 275270.
                   07  COLLEGE-ID      PIC X(3)   VALUE '461'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66100.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0058A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001242.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MONTEREY PENINSULA COLLEGE'.
               05  NAPA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 285540.
                   07  COLLEGE-ID      PIC X(3)   VALUE '241'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66274.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0062A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001247.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'NAPA VALLEY COLLEGE'.
               05  COASTLINE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305001.
                   07  COLLEGE-ID      PIC X(3)   VALUE '831'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66639.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0104A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 029027.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'COASTLINE COMMUNITY COLLEGE'.
               05  CYPRESS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305191.
                   07  COLLEGE-ID      PIC X(3)   VALUE '861'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 30555.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0022A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001193.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CYPRESS COLLEGE'.
               05  NORTH-ORANGE-ADULT.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305230.
                   07  COLLEGE-ID      PIC X(3)   VALUE '863'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 30555.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0032A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'NORTH ORANGE ADULT DIVISION'.
               05  FULLERTON.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305240.
                   07  COLLEGE-ID      PIC X(3)   VALUE '862'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 30555.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0032A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001201.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'FULLERTON COLLEGE'.
               05  GOLDEN-WEST.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305282.
                   07  COLLEGE-ID      PIC X(3)   VALUE '832'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66639.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0035A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001206.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'GOLDEN WEST COLLEGE'.
               05  ORANGE-COAST.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305525.
                   07  COLLEGE-ID      PIC X(3)   VALUE '833'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66639.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0064A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001250.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'ORANGE COAST COLLEGE'.
               05  SADDLEBACK.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305579.
                   07  COLLEGE-ID      PIC X(3)   VALUE '891'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66654.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0076A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008918.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SADDLEBACK COLLEGE'.
               05  IRVINE-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305580.
                   07  COLLEGE-ID      PIC X(3)   VALUE '892'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66654.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0076A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'IRVINE VALLEY COLLEGE'.
               05  RANCHO-SANTIAGO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305609.
                   07  COLLEGE-ID      PIC X(3)   VALUE '871'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66688.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0086A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001284.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'RANCHO SANTIAGO COLLEGE'.
               05  RANCHO-SANTIAGO-CE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 305620.
                   07  COLLEGE-ID      PIC X(3)   VALUE '872'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66688.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0086A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SANTA ANA CONTINUING EDUCATION'.
               05  SIERRA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 315730.
                   07  COLLEGE-ID      PIC X(3)   VALUE '271'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 66936.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0092A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001290.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SIERRA COLLEGE'.
               05  FEATHER-RIVER.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 325225.
                   07  COLLEGE-ID      PIC X(3)   VALUE '121'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 75143.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0029A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008597.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'FEATHER RIVER COLLEGE'.
               05  COLLEGE-OF-THE-DESERT.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 335125.
                   07  COLLEGE-ID      PIC X(3)   VALUE '931'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67025.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0024A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001182.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'DESERT, COLLEGE OF THE'.
               05  MT-SAN-JACINTO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 335403.
                   07  COLLEGE-ID      PIC X(3)   VALUE '941'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67132.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0061A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001246.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MOUNT SAN JACINTO COLLEGE'.
               05  PALO-VERDE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 335565.
                   07  COLLEGE-ID      PIC X(3)   VALUE '951'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73510.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0067A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001259.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'PALO VERDE COLLEGE'.
               05  RIVERSIDE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 335687.
                   07  COLLEGE-ID      PIC X(3)   VALUE '961'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67223.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0074A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001270.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'RIVERSIDE COMMUNITY COLLEGE'.
               05  AMERICAN-RIVER.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 345023.
                   07  COLLEGE-ID      PIC X(3)   VALUE '231'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67371.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0003A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 009552.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'AMERICAN RIVER COLLEGE'.
               05  COSUMNES-RIVER.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 345124.
                   07  COLLEGE-ID      PIC X(3)   VALUE '232'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67371.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0019A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007536.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'COSUMNES RIVER COLLEGE'.
               05  SACRAMENTO-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 345740.
                   07  COLLEGE-ID      PIC X(3)   VALUE '233'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67371.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0075A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001233.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SACRAMENTO CITY COLLEGE'.
               05  BARSTOW.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 365074.
                   07  COLLEGE-ID      PIC X(3)   VALUE '911'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67629.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0006A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001119.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'BARSTOW COLLEGE'.
               05  CHAFFEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 365210.
                   07  COLLEGE-ID      PIC X(3)   VALUE '921'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67660.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0014A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 01163.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CHAFFEY COLLEGE'.
               05  CRAFTON-HILLS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 365211.
                   07  COLLEGE-ID      PIC X(3)   VALUE '981'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67884.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0020A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 009272.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CRAFTON HILLS COLLEGE'.
               05  SAN-BERNARDINO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 365594.
                   07  COLLEGE-ID      PIC X(3)   VALUE '982'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67884.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0077A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001272.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN BERNARDINO VALLEY COLLEGE'.
               05  VICTOR-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 365790.
                   07  COLLEGE-ID      PIC X(3)   VALUE '991'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 67926.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0099A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001335.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'VICTOR VALLEY COMMUNITY COLLEG'.
               05  GROSSMONT.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375249.
                   07  COLLEGE-ID      PIC X(3)   VALUE '022'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68148.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0036A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001208.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'GROSSMONT COLLEGE'.
               05  CUYAMACA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375250.
                   07  COLLEGE-ID      PIC X(3)   VALUE '021'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68148.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0106A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 029246.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CUYAMACA COLLEGE'.
               05  MIRA-COSTA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375509.
                   07  COLLEGE-ID      PIC X(3)   VALUE '051'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68247.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0056A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001239.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MIRA COSTA COLLEGE'.
               05  PALOMAR.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375542.
                   07  COLLEGE-ID      PIC X(3)   VALUE '061'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68270.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0066A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001260.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'PALOMAR COLLEGE'.
               05  SAN-DIEGO-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375663.
                   07  COLLEGE-ID      PIC X(3)   VALUE '071'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0078A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 008895.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO CITY COLLEGE'.
               05  SAN-DIEGO-MESA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375693.
                   07  COLLEGE-ID      PIC X(3)   VALUE '072'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0080A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001275.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO MESA COLLEGE'.
               05  SAN-DIEGO-MIRAMAR.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375300.
                   07  COLLEGE-ID      PIC X(3)   VALUE '073'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0081A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 012662.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO MIRAMAR COLLEGE REG'.
               05  SAN-DIEGO-ADULT.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375700.
                   07  COLLEGE-ID      PIC X(3)   VALUE '076'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO ADULT/CONTINUING EDU'.
               05  SAN-DIEGO-ED-CULT-CMPLX.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375997.
                   07  COLLEGE-ID      PIC X(3)   VALUE '077'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007478.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO ED CULTURAL CMPLX'.
               05  SAN-DIEGO-ADULT-2.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375998.
                   07  COLLEGE-ID      PIC X(3)   VALUE '078'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007478.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO ADULT/CONTINUING EDU'.
               05  SAN-DIEGO-ADULT-MILITARY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375999.
                   07  COLLEGE-ID      PIC X(3)   VALUE '079'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73528.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007478.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN DIEGO MILITARY ED PROGRAM'.
               05  SOUTHWESTERN.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 375807.
                   07  COLLEGE-ID      PIC X(3)   VALUE '091'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68429.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0096A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001294.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SOUTHWESTERN COLLEGE'.
               05  SAN-FRANCISCO-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 385092.
                   07  COLLEGE-ID      PIC X(3)   VALUE '361'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73536.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0082A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001167.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN FRANCISCO, CITY COLLEGE OF'.
               05  SAN-FRANCISCO-CNTR.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 385099.
                   07  COLLEGE-ID      PIC X(3)   VALUE '363'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 73536.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001167.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN FRANCISCO CMTY CLG CENTERS'.
               05  SAN-JOAQUIN-DELTA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 395670.
                   07  COLLEGE-ID      PIC X(3)   VALUE '551'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68668.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0083A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001280.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN JOAQUIN DELTA COLLEGE'.
               05  CUESTA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 405650.
                   07  COLLEGE-ID      PIC X(3)   VALUE '641'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 68817.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0021A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001192.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CUESTA COLLEGE'.
               05  CANADA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 415062.
                   07  COLLEGE-ID      PIC X(3)   VALUE '371'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69054.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0009A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 006973.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CANADA COLLEGE'.
               05  SAN-MATEO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 415151.
                   07  COLLEGE-ID      PIC X(3)   VALUE '372'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69054.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0085A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001181.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN MATEO, COLLEGE OF'.
               05  SKYLINE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 415711.
                   07  COLLEGE-ID      PIC X(3)   VALUE '373'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69054.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0094A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007713.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SKYLINE COLLEGE'.
               05  ALLAN-HANCOCK.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 425213.
                   07  COLLEGE-ID      PIC X(3)   VALUE '611'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69096.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0002A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001111.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'ALLAN HANCOCK COLLEGE'.
               05  SANTA-BARBARA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 425560.
                   07  COLLEGE-ID      PIC X(3)   VALUE '651'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69294.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0087A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001285.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SANTA BARBARA CITY COLLEGE'.
               05  SANTA-BARBARA-CED.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 425561.
                   07  COLLEGE-ID      PIC X(3)   VALUE '652'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69294.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE SPACES.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SANTA BARBARA CONTINUING EDUCA'.
               05  DE-ANZA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435184.
                   07  COLLEGE-ID      PIC X(3)   VALUE '421'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69443.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0023A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 004480.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'DE ANZA COLLEGE'.
               05  FOOTHILL.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435227.
                   07  COLLEGE-ID      PIC X(3)   VALUE '422'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69443.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0030A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001199.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'FOOTHILL COLLEGE'.
               05  GAVILAN.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435263.
                   07  COLLEGE-ID      PIC X(3)   VALUE '441'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69476.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0033A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001202.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'GAVILAN COLLEGE'.
               05  EVERGREEN.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435679.
                   07  COLLEGE-ID      PIC X(3)   VALUE '471'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69658.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0028A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 012452.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'EVERGREEN VALLEY COLLEGE'.
               05  SAN-JOSE-CITY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435680.
                   07  COLLEGE-ID      PIC X(3)   VALUE '472'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69658.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0084A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001282.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SAN JOSE CITY COLLEGE'.
               05  W-VALLEY-COMM-SERV.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435000.
                   07  COLLEGE-ID      PIC X(3)   VALUE '491'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69716.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0102A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE ZEROES.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'WEST VALLEY COMMUNITY SERVICES'.
               05  MISSION.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435861.
                   07  COLLEGE-ID      PIC X(3)   VALUE '492'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69716.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0105A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001338.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MISSION COLLEGE'.
               05  WEST-VALLEY.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 435860.
                   07  COLLEGE-ID      PIC X(3)   VALUE '493'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69716.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0102A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001338.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'WEST VALLEY COLLEGE'.
               05  CABRILLO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 445076.
                   07  COLLEGE-ID      PIC X(3)   VALUE '411'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 69740.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0008A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001124.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'CABRILLO COLLEGE'.
               05  SHASTA-COLLEGE.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 455695.
                   07  COLLEGE-ID      PIC X(3)   VALUE '171'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70144.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0091A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001289.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SHASTA COLLEGE'.
               05  SISKIYOUS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 475200.
                   07  COLLEGE-ID      PIC X(3)   VALUE '181'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70474.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0093A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001187.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SISKIYOUS, COLLEGE OF THE'.
               05  SOLANO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 485825.
                   07  COLLEGE-ID      PIC X(3)   VALUE '281'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70557.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0095A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001292.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SOLANO COMMUNITY COLLEGE'.
               05  SANTA-ROSA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 495690.
                   07  COLLEGE-ID      PIC X(3)   VALUE '261'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 70946.
                   07  CPEC-COLLEGE-CODE
                                       PIC X(6)   VALUE 'C0089A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001287.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SANTA ROSA JUNIOR COLLEGE'.
               05  SEQUOIAS.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 545071.
                   07  COLLEGE-ID      PIC X(3)   VALUE '561'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72124.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0090A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001186.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'SEQUOIAS, COLLEGE OF THE'.
               05  COLUMBIA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 555055.
                   07  COLLEGE-ID      PIC X(3)   VALUE '591'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 71340.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0016A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007707.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'COLUMBIA COLLEGE'.
               05  MODESTO.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 505500.
                   07  COLLEGE-ID      PIC X(3)   VALUE '592'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 71340.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0057A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001240.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MODESTO JUNIOR COLLEGE'.
               05  MOORPARK.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 565320.
                   07  COLLEGE-ID      PIC X(3)   VALUE '681'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72660.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0059A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 007115.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'MOORPARK COLLEGE'.
               05  OXNARD.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 565321.
                   07  COLLEGE-ID      PIC X(3)   VALUE '682'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72660.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0065A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)  VALUE 012842.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'OXNARD COLLEGE'.
               05  VENTURA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 565741.
                   07  COLLEGE-ID      PIC X(3)   VALUE '683'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72660.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0098A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001334.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'VENTURA COLLEGE'.
               05  YUBA.
                   07  COLLEGE-CODE    PIC 9(6)   VALUE 585925.
                   07  COLLEGE-ID      PIC X(3)   VALUE '291'.
                   07  DISTRICT-CODE   PIC 9(5)   VALUE 72777.
                   07  CPEC-NUMERIC-CODE
                                       PIC X(6)   VALUE 'C0103A'.
                   07  FICE-COLLEGE-CODE
                                       PIC 9(6)   VALUE 001344.
                   07  COLLEGE-NAME    PIC X(30)
                           VALUE 'YUBA COLLEGE'.
           03  COLLEGE-CODE-ENTRY
                       REDEFINES COLLEGE-CODE-VALUES
                       OCCURS 119 TIMES
                       INDEXED BY COLLEGE-CODE-INDEX.
               05  COLLEGE-CODE        PIC 9(6).
               05  COLLEGE-ID          PIC X(3).
               05  DISTRICT-CODE       PIC 9(5).
               05  CPEC-NUMERIC-CODE   PIC X(6).
               05  FICE-COLLEGE-CODE   PIC 9(6).
               05  COLLEGE-NAME        PIC X(30).
      *
      *
       01  HEADER-WORKAREA.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *                 H E A D E R   W O R K A R E A                 *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *
           03  HEADING-LINE            PIC X(133) VALUE SPACES.
           03  HEADING-TITLE-LINE
                       REDEFINES HEADING-LINE.
               05  FILLER              PIC X(29).
               05  WIDE-HEADING-LOCATION
                                       PIC X(75).
               05  FILLER              PIC X(29).
           03  HEADING-STATISTICS-LINE
                       REDEFINES HEADING-LINE.
               05  FILLER              PIC X.
               05  LEFT-STATISTIC      PIC X(31).
               05  FILLER              PIC X(26).
               05  NARROW-RIGHT-STATISTIC
                                       PIC X(18).
               05  FILLER              PIC X(39).
               05  WIDE-RIGHT-STATISTIC
                                       PIC X(18).
           03  HEADER-STATISTICS.
               05  HEADER-REPORT-PAGE.
                   07  FILLER          PIC X(8)   VALUE SPACES.
                   07  FILLER          PIC X(5)   VALUE 'PAGE '.
                   07  HEADER-PAGE-NUMBER
                                       PIC Z(5)   VALUE SPACES.
               05  HEADER-REPORT-DATE.
                   07  HEADER-MONTH    PIC X(9)   VALUE SPACES.
                   07  FILLER          PIC X      VALUE SPACE.
                   07  HEADER-DAY      PIC Z(2)   VALUE SPACES.
                   07  FILLER          PIC X(4)   VALUE ', 19'.
                   07  HEADER-YEAR     PIC Z(2)   VALUE SPACES.
               05  HEADER-REPORT-TIME.
                   07  FILLER          PIC X(6)   VALUE 'TIME'.
                   07  HEADER-TIME-AREA           VALUE SPACES.
                       09  HEADER-TIME PIC Z9/99.
               05  HEADER-REPORT-ID.
                   07  FILLER          PIC X(11)  VALUE 'REPORT-ID: '.
                   07  PROGRAM-NAME    PIC X(20)  VALUE SPACES.
           03  HEADER-MONTH-NAMES.
               05  JANUARY.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '  JANUARY'.
               05  FEBRUARY.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE ' FEBRUARY'.
               05  MARCH.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '    MARCH'.
               05  APRIL.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '    APRIL'.
               05  MAY.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '      MAY'.
               05  JUNE.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '     JUNE'.
               05  JULY.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '     JULY'.
               05  AUGUST.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '   AUGUST'.
               05  SEPTEMBER.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE 'SEPTEMBER'.
               05  OCTOBER.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE '  OCTOBER'.
               05  NOVEMBER.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE ' NOVEMBER'.
               05  DECEMBER.
                   07  FULL-MONTH-NAME PIC X(9)   VALUE ' DECEMBER'.
           03  HEADER-MONTH-TABLE
                       REDEFINES HEADER-MONTH-NAMES
                       OCCURS 12 TIMES
                       INDEXED BY HEADER-MONTH-INDEX.
               05 HEADER-MONTH-NAME.
                   07  FULL-MONTH-NAME PIC X(9).
           03  HEADER-HOLDING-AREA.
               05  REPORT-DATE         PIC 9(6).
               05  EXPANDED-REPORT-DATE
                           REDEFINES REPORT-DATE.
                   07  REPORT-YEAR     PIC 9(2).
                   07  REPORT-MONTH    PIC 9(2).
                   07  REPORT-DAY      PIC 9(2).
               05  REPORT-TIME         PIC 9(8).
               05  EXPANDED-REPORT-TIME
                           REDEFINES REPORT-TIME.
                   07  ACTUAL-CLOCK-TIME
                                       PIC 9(4).
                   07  FILLER          PIC 9(4).
               05  MIS-PILOT-TITLE.
                   07  FILLER          PIC X(23)  VALUE SPACES.
                   07  FILLER          PIC X(11)  VALUE 'MANAGEMENT'.
                   07  FILLER          PIC X(12)  VALUE 'INFORMATION'.
                   07  FILLER          PIC X(6)   VALUE 'SYSTEM'.
               05  COMMUNITY-COLLEGE-TITLE.
                   07  FILLER          PIC X(9)   VALUE SPACES.
                   07  FILLER          PIC X(12)  VALUE 'C A L I F O '.
                   07  FILLER          PIC X(12)  VALUE 'R N I A   C '.
                   07  FILLER          PIC X(12)  VALUE 'O M M U N I '.
                   07  FILLER          PIC X(12)  VALUE 'T Y   C O L '.
                   07  FILLER          PIC X(9)   VALUE 'L E G E S'.
               05  MAXIMUM-LINES-PER-PAGE
                                       PIC 9(2)   VALUE 59.
           03  HEADER-SWITCHES-AND-FLAGS.
               05  FIRST-PAGE-SWITCH   PIC 9      VALUE 1.
                   88  FIRST-PAGE                 VALUE 1.
               05  FIRST-PAGE-FLAG     PIC 9      VALUE 1.
               05  REPORT-WIDTH-SWITCH PIC X      VALUE 'W'.
                   88  WIDE-PAPER-REQUIRED        VALUE 'W'.
                   88  NARROW-PAPER-REQUIRED      VALUE 'N'.
               05  WIDE-PAPER-FLAG     PIC X      VALUE 'W'.
               05  NARROW-PAPER-FLAG   PIC X      VALUE 'N'.
           03  HEADER-WORKAREA-COUNTERS           VALUE ZEROS.
               05  REPORT-PAGE-COUNTER PIC 9(5).
               05  REPORT-LINE-COUNTER PIC 9(2).
                   88  TOP-OF-PAGE                VALUE ZEROS.
      *
      *
       LINKAGE SECTION.
      *
       01  RUNTIME-PARAMETER-AREA.
           03  FILLER                  PIC 9(3) COMP.
           03  PRT-CONTROL             PIC X(3).
               88  PRT-CONTROL-VALID            VALUE '000' THRU '999'
                                                      'ALL'.
               88  PRINT-ALL-PAGES              VALUE 'ALL'.
           03  FILLER                  PIC X.
           03  TERM-CHECK.
               05  CALENDAR-YEAR       PIC 9(2).
               05  TERM-CODE           PIC 9(1).
           03  FILLER                  PIC X.
           03  DISTRICT-ID             PIC 9(3).
           03  PARAMETER-COLLEGES
                       OCCURS 10 TIMES
                       INDEXED BY PARAMETER-COLLEGE-INDEX.
               05  FILLER              PIC X(1).
               05  COLLEGE-ID          PIC X(3).
      *
      /
       PROCEDURE DIVISION USING RUNTIME-PARAMETER-AREA.
      *
       0000-PROGRAM-DRIVER.
           PERFORM 1000-PROGRAM-INITIALIZATION.
           PERFORM 2000-PROCESS-COURSE-DATA
                   UNTIL END-OF-FILE.
           PERFORM 9000-PROGRAM-FINALIZATION.
           STOP RUN.
      *
      *
       1000-PROGRAM-INITIALIZATION.
           MOVE ZEROS TO PROGRAM-FLAG-AREA.
           MOVE ZEROS TO PROGRAM-ACCUMULATORS.
           MOVE ZEROS TO DATA-VALIDATION-FLAGS.
           OPEN OUTPUT DATAEDIT-ERROR-REPORT.
           PERFORM 1100-PROCESS-PARAMETER-INPUT.
           OPEN INPUT COURSE-INV-DATA-FILE.
           OPEN INPUT TOPCODE-CHECK-FILE.
           PERFORM 1102-SET-TOPCODE-TABLE.
           OPEN OUTPUT EDITED-COURSE-INV-DATA-FILE.
           OPEN OUTPUT REPORT-TOTALS-FILE.
           OPEN OUTPUT SUMMARY-ERROR-REPORT.
           PERFORM 8000-READ-COURSE-INV-DATA.
           MOVE COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   TO HOLD-COLLEGE-ID.
           MOVE CALENDAR-YEAR
                   IN RUNTIME-PARAMETER-AREA
                   TO CALENDAR-YEAR
                   IN REPORT-TITLE-LINE-2.
           MOVE CALENDAR-YEAR
                   IN RUNTIME-PARAMETER-AREA
                   TO YEAR-OUT
                   IN REPORT-TITLE-LINE-2.
           MOVE TERM-CODE
                   IN RUNTIME-PARAMETER-AREA
                   TO TERM-CODE
                   IN REPORT-TITLE-LINE-2.
           IF  NOT PRINT-ALL-PAGES
               MOVE PRT-CONTROL
                       TO PRT-PAGE-MAXIMUM.
           MOVE 1 TO NEW-COLLEGE-FLAG.
           MOVE 1 TO PAGE-FULL-FLAG.
           SET EDIT-ERROR-COLUMN-INDEX TO 1.
           MOVE SPACES TO EDIT-ERROR-TABLE.
      *
      *
       1100-PROCESS-PARAMETER-INPUT.
           IF  TERM-CHECK IS EQUAL TO ZEROS
                   OR TERM-CHECK IS NOT NUMERIC
               MOVE 1 TO TERM-ID-FLAG
           ELSE
               PERFORM 1110-VALIDATE-TERM-ID.
           IF  DISTRICT-ID
                   IN RUNTIME-PARAMETER-AREA
                   IS EQUAL TO ZEROS
                   OR DISTRICT-ID
                   IN RUNTIME-PARAMETER-AREA
                   IS NOT NUMERIC
               MOVE 1 TO DISTRICT-ID-FLAG
           ELSE
               PERFORM 1140-VALIDATE-DISTRICT-ID.
           MOVE SPACES TO RUNTIME-PARAMETER-TABLE.
           SET PARAMETER-COLLEGE-INDEX TO 1.
           PERFORM 1120-VALIDATE-COLLEGE-PARAMS
                   UNTIL TABLE-SEARCH-COMPLETED.
           IF  COLLEGE-ID-INVALID
                   OR NOT (PRT-CONTROL-VALID)
                   OR TERM-ID-INVALID
                   OR DISTRICT-ID-INVALID
                   OR RUNTIME-PARAMETER-TABLE
                           IS EQUAL TO SPACES
               PERFORM 1130-KILL-PROCESS.
      *
      *
       1102-SET-TOPCODE-TABLE.
           PERFORM 1104-LOAD-TOPCODE-TABLE
                VARYING TOPIDX FROM 1 BY 1
                UNTIL TOP-EOF-FLAG IS EQUAL TO 1.
      *
      *
       1104-LOAD-TOPCODE-TABLE.
           READ TOPCODE-CHECK-FILE
               AT END MOVE 1 TO TOP-EOF-FLAG.
               IF TOP-EOF-FLAG IS NOT EQUAL TO 1
                   MOVE TOPCODE TO TOP-CODE-Y (TOPIDX).
      *
      *
       1110-VALIDATE-TERM-ID.
           SET TERM-IDENTIFIER-INDEX TO 1.
           SEARCH TERM-IDENTIFIER-ENTRY
                   IN TERM-IDENTIFIER-TABLE
           AT END
               MOVE 1 TO TERM-ID-FLAG
           WHEN TERM-CODE
                   IN TERM-IDENTIFIER-ENTRY
                   (TERM-IDENTIFIER-INDEX)
                   IS EQUAL TO TERM-CODE
                   IN RUNTIME-PARAMETER-AREA
               MOVE TERM-TITLE
                       IN TERM-IDENTIFIER-ENTRY
                       (TERM-IDENTIFIER-INDEX)
                       TO TERM-TITLE
                       IN REPORT-TITLE-LINE-2.
      *
      *
       1120-VALIDATE-COLLEGE-PARAMS.
           IF COLLEGE-ID
                   IN PARAMETER-COLLEGES
                   (PARAMETER-COLLEGE-INDEX)
                   IS EQUAL TO SPACES
               NEXT SENTENCE
           ELSE
               PERFORM 1121-SEARCH-COLLEGE-TABLE
           IF COLLEGE-NOT-FOUND
               MOVE 1 TO COLLEGE-ID-FLAG
               MOVE 1 TO TABLE-SEARCH-FLAG
           ELSE
               PERFORM 1122-FILL-WORK-TABLE.
           SET ORIGINAL-INDEX-VALUE
                   TO PARAMETER-COLLEGE-INDEX.
           SEARCH PARAMETER-COLLEGES
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO PARAMETER-COLLEGE-INDEX
               NEXT SENTENCE.
      *
      *
       1121-SEARCH-COLLEGE-TABLE.
           SET COLLEGE-CODE-INDEX TO 1.
           SEARCH COLLEGE-CODE-ENTRY
                   IN COLLEGE-CODES-TABLE
           AT END
               MOVE 1 TO COLLEGE-SEARCH-FLAG
           WHEN COLLEGE-ID
                    IN COLLEGE-CODE-ENTRY
                    (COLLEGE-CODE-INDEX)
                    IS EQUAL TO COLLEGE-ID
                    IN PARAMETER-COLLEGES
                    (PARAMETER-COLLEGE-INDEX)
               MOVE ZERO TO COLLEGE-SEARCH-FLAG.
      *
      *
       1122-FILL-WORK-TABLE.
           SET RUNTIME-PARAMETER-INDEX TO 1.
           SEARCH RUNTIME-PARAMETER-COLLEGES
           AT END
               MOVE 1 TO COLLEGE-ID-FLAG
           WHEN COLLEGE-ID
                   IN RUNTIME-PARAMETER-TABLE
                   (RUNTIME-PARAMETER-INDEX)
                   IS EQUAL TO SPACES
               MOVE COLLEGE-ID
                       IN PARAMETER-COLLEGES
                       (PARAMETER-COLLEGE-INDEX)
                       TO COLLEGE-ID
                       IN RUNTIME-PARAMETER-TABLE
                       (RUNTIME-PARAMETER-INDEX)
               MOVE COLLEGE-NAME
                       IN COLLEGE-CODE-ENTRY
                       (COLLEGE-CODE-INDEX)
                       TO COLLEGE-NAME
                       IN RUNTIME-PARAMETER-TABLE
                       (RUNTIME-PARAMETER-INDEX).
      *
      *
       1130-KILL-PROCESS.
           MOVE 1 TO HEADER-CONTROL-FLAG.
           MOVE DETAIL-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 8500-PRINT-HEADERS.
           WRITE PRINT-RECORD
               FROM RUNTIME-PARAMETER-AREA
               AFTER ADVANCING 2 LINES.
           IF  NOT PRT-CONTROL-VALID
               WRITE PRINT-RECORD
                   FROM PRT-CNTL-INVALID-LITERAL
                   AFTER ADVANCING 1 LINE.
           IF  TERM-ID-INVALID
               WRITE PRINT-RECORD
                   FROM TERM-ID-INVALID-LITERAL
                   AFTER ADVANCING 1 LINE.
           IF  DISTRICT-ID-INVALID
               WRITE PRINT-RECORD
                   FROM DISTRICT-ID-INVALID-LITERAL
                   AFTER ADVANCING 1 LINE.
           IF  COLLEGE-ID-INVALID
                   OR RUNTIME-PARAMETER-TABLE
                   IS EQUAL TO SPACES
               WRITE PRINT-RECORD
                   FROM COLLEGE-INVALID-LITERAL
                   AFTER ADVANCING 1 LINE.
           CLOSE DATAEDIT-ERROR-REPORT.
           STOP RUN.
      *
      *
       1140-VALIDATE-DISTRICT-ID.
           SET DISTRICT-CODE-INDEX TO 1.
           SEARCH DISTRICT-CODE-ENTRY
           AT END
               MOVE 1 TO DISTRICT-ID-FLAG
           WHEN DISTRICT-ID
                   IN DISTRICT-CODE-ENTRY
                   (DISTRICT-CODE-INDEX)
                   IS EQUAL TO DISTRICT-ID
                   IN RUNTIME-PARAMETER-AREA
               NEXT SENTENCE.
      *
      *
       2000-PROCESS-COURSE-DATA.
           IF  COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
               IF  COLLEGE-ID
                       IN COURSE-INVENTORY-RECORD
                       IS NOT EQUAL TO HOLD-COLLEGE-ID
                   PERFORM 2200-PROCESS-CNTL-BREAK.
           ADD 1 TO TOTAL-COLLEGE-RECORDS-READ.
           PERFORM 2100-VALIDATE-ID-FIELDS.
           PERFORM 2300-VALIDATE-COURSE-DATA.
      *    PERFORM 2500-ACCUMULATE-UNKNOWNS.
           IF  REJECT-RECORD
               IF  PRINT-ALL-PAGES
                       PERFORM 2600-FILL-ERROR-MATRIX
               ELSE
               IF  (REPORT-PAGE-COUNTER IS > PRT-PAGE-MAXIMUM
                       OR  REPORT-PAGE-COUNTER IS = PRT-PAGE-MAXIMUM)
                       AND PAGE-FULL
                   MOVE SPACES TO EDIT-ERROR-TABLE
               ELSE
                   PERFORM 2600-FILL-ERROR-MATRIX.
           IF  NOT REJECT-RECORD
               PERFORM 8600-WRITE-COURSE-DATA
           ELSE
               ADD 1 TO TOTAL-COLLEGE-RECORDS-REJECT.
           MOVE ZERO TO DATA-VALIDATION-FLAGS.
           PERFORM 8000-READ-COURSE-INV-DATA.
      *
      *
       2100-VALIDATE-ID-FIELDS.
           IF  NOT COURSE-DATA-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (3 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (3 EDIT-ERROR-COLUMN-INDEX).
           IF  NEW-COLLEGE
                   AND COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
               MOVE ZERO TO NEW-COLLEGE-FLAG
               MOVE ZERO TO COLLEGE-SEARCH-FLAG
               PERFORM 2110-SEARCH-PARAMETER-TABLE.
           IF  COLLEGE-NOT-FOUND
                   OR COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (4 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (4 EDIT-ERROR-COLUMN-INDEX).
           IF  (TERM-IDENTIFIER
                   IN COURSE-INVENTORY-RECORD
                   IS NOT NUMERIC)
                   OR (CALENDAR-YEAR
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO CALENDAR-YEAR
                   IN RUNTIME-PARAMETER-AREA)
                   OR (TERM-CODE
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO TERM-CODE
                   IN RUNTIME-PARAMETER-AREA)
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (5 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                    TO VALIDATION-FLAG
                    IN EDIT-ERROR-TABLE-COLUMNS
                    (5 EDIT-ERROR-COLUMN-INDEX).
           IF  COURSE-PERM-DIST-ID
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (1 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (1 EDIT-ERROR-COLUMN-INDEX).
           IF COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (2 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (2 EDIT-ERROR-COLUMN-INDEX)
               MOVE 0 TO COURSE-ID-FLAG
               ELSE
                  MOVE 1 TO COURSE-ID-FLAG.
      *
      *
       2110-SEARCH-PARAMETER-TABLE.
           SET RUNTIME-PARAMETER-INDEX TO 1.
           SEARCH RUNTIME-PARAMETER-COLLEGES
           AT END
               MOVE 1 TO COLLEGE-SEARCH-FLAG
           WHEN COLLEGE-ID
                   IN RUNTIME-PARAMETER-TABLE
                   (RUNTIME-PARAMETER-INDEX)
                   IS EQUAL TO COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
               MOVE COLLEGE-NAME
                       IN RUNTIME-PARAMETER-TABLE
                       (RUNTIME-PARAMETER-INDEX)
                       TO COLLEGE-NAME
                       IN REPORT-TITLE-LINE-3
               MOVE ZERO TO COLLEGE-SEARCH-FLAG.
      *
      *
       2200-PROCESS-CNTL-BREAK.
           IF  EDIT-ERROR-TABLE
                   IS NOT EQUAL TO SPACES
               PERFORM 8400-PRINT-ERROR-REPORT.
           MOVE 1 TO PAGE-FULL-FLAG.
           MOVE 1 TO NEW-COLLEGE-FLAG.
           IF  NOT SINGLE-COLLEGE-DISTRICT
                   IN DISTRICT-CODE-ENTRY
                   (DISTRICT-CODE-INDEX)
               PERFORM 8200-PRINT-COLLEGE-TOTALS.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET COLLEGE-ELEMENT-ROW-INDEX TO 1.
           SET DISTRICT-ELEMENT-ROW-INDEX TO 1.
           PERFORM 2210-ADD-DISTRICT-ELMNT-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET TOTAL-COLLEGE-INDEX TO 1.
           SET TOTAL-DISTRICT-INDEX TO 1.
           PERFORM 2220-ADD-DISTRICT-SUM-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
           PERFORM 2230-ADD-DISTRICT-INTGY-TOTALS.
           PERFORM 2240-WRITE-RPT-TTLS-RECDS-FILE.
           MOVE COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   TO HOLD-COLLEGE-ID.
           MOVE ZEROS TO TOTAL-COLLEGE-ENTRIES
                   IN PROGRAM-ACCUMULATORS.
           MOVE ZEROS TO COLLEGE-ELEMENT-TOTALS
                   IN PROGRAM-ACCUMULATORS.
           MOVE ZEROS TO COLLEGE-INTEGRITY-TOTALS
                   IN PROGRAM-ACCUMULATORS.
      *
      *
       2210-ADD-DISTRICT-ELMNT-TOTALS.
           SET COLLEGE-ELEMENT-COLUMN-INDEX TO 1.
           SET DISTRICT-ELEMENT-COLUMN-INDEX TO 1.
           MOVE ZERO TO ADD-ELEMENT-COLUMNS-FLAG.
           PERFORM 2211-ADD-TO-DISTRICT-MATRIX
                   UNTIL ADD-ELEMENT-COLUMNS-DONE.
           SET ORIGINAL-INDEX-VALUE
                   TO COLLEGE-ELEMENT-ROW-INDEX.
           SEARCH COLLEGE-ELEMENT-TOTALS-ROWS
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO COLLEGE-ELEMENT-ROW-INDEX
               NEXT SENTENCE.
           SET EDIT-ERROR-LITERAL-INDEX
                   TO COLLEGE-ELEMENT-ROW-INDEX.
           SET DISTRICT-ELEMENT-ROW-INDEX
                   TO COLLEGE-ELEMENT-ROW-INDEX.
      *
      *
       2211-ADD-TO-DISTRICT-MATRIX.
           ADD COLLEGE-ELEMENT-TOTAL
                   IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                   (COLLEGE-ELEMENT-ROW-INDEX
                    COLLEGE-ELEMENT-COLUMN-INDEX)
                   TO DISTRICT-ELEMENT-TOTAL
                   IN DISTRICT-ELEMNT-TOTALS-COLUMNS
                   (DISTRICT-ELEMENT-ROW-INDEX
                    DISTRICT-ELEMENT-COLUMN-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO COLLEGE-ELEMENT-COLUMN-INDEX.
           SEARCH COLLEGE-ELEMENT-TOTALS-COLUMNS
           AT END
               MOVE 1 TO ADD-ELEMENT-COLUMNS-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO COLLEGE-ELEMENT-COLUMN-INDEX
               NEXT SENTENCE.
           SET DISTRICT-ELEMENT-COLUMN-INDEX
                   TO COLLEGE-ELEMENT-COLUMN-INDEX.
      *
      *
       2220-ADD-DISTRICT-SUM-TOTALS.
           ADD COLLEGE-TOTAL
                   IN TOTAL-COLLEGE-ENTRY
                   (TOTAL-COLLEGE-INDEX)
                   TO DISTRICT-TOTAL
                   IN TOTAL-DISTRICT-ENTRY
                   (TOTAL-DISTRICT-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO TOTAL-COLLEGE-INDEX.
           SEARCH TOTAL-COLLEGE-ENTRY
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO TOTAL-COLLEGE-INDEX
               NEXT SENTENCE.
           SET TOTAL-DISTRICT-INDEX
                   TO TOTAL-COLLEGE-INDEX.
      *
      *
       2230-ADD-DISTRICT-INTGY-TOTALS.
           ADD CLG-INTEGRITY-ERR-1-TOTAL TO DIST-INTEGRITY-ERR-1-TOTAL.
           ADD CLG-INTEGRITY-ERR-2-TOTAL TO DIST-INTEGRITY-ERR-2-TOTAL.
           ADD CLG-INTEGRITY-ERR-3-TOTAL TO DIST-INTEGRITY-ERR-3-TOTAL.
           ADD CLG-INTEGRITY-ERR-4-TOTAL TO DIST-INTEGRITY-ERR-4-TOTAL.
           ADD CLG-INTEGRITY-ERR-5-TOTAL TO DIST-INTEGRITY-ERR-5-TOTAL.
           ADD CLG-INTEGRITY-ERR-6-TOTAL TO DIST-INTEGRITY-ERR-6-TOTAL.
           ADD CLG-INTEGRITY-ERR-7-TOTAL TO DIST-INTEGRITY-ERR-7-TOTAL.
           ADD CLG-INTEGRITY-ERR-8-TOTAL TO DIST-INTEGRITY-ERR-8-TOTAL.
           ADD CLG-INTEGRITY-ERR-9-TOTAL TO DIST-INTEGRITY-ERR-9-TOTAL.
           ADD CLG-INTEGRITY-ERR-10-TOTAL
                             TO DIST-INTEGRITY-ERR-10-TOTAL.
           ADD CLG-INTEGRITY-ERR-11-TOTAL
                             TO DIST-INTEGRITY-ERR-11-TOTAL.
           ADD CLG-INTEGRITY-ERR-12-TOTAL
                             TO DIST-INTEGRITY-ERR-12-TOTAL.
           ADD CLG-INTEGRITY-ERR-13-TOTAL
                             TO DIST-INTEGRITY-ERR-13-TOTAL.
           ADD CLG-INTEGRITY-ERR-14-TOTAL
                             TO DIST-INTEGRITY-ERR-14-TOTAL.
           ADD CLG-INTEGRITY-ERR-15-TOTAL
                             TO DIST-INTEGRITY-ERR-15-TOTAL.
           ADD CLG-INTEGRITY-ERR-16-TOTAL
                             TO DIST-INTEGRITY-ERR-16-TOTAL.
      *    ADD CLG-INTEGRITY-ERR-17-TOTAL
      *                      TO DIST-INTEGRITY-ERR-17-TOTAL.
      *
      *
       2240-WRITE-RPT-TTLS-RECDS-FILE.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET COLLEGE-ELEMENT-ROW-INDEX TO 1.
           SET EDIT-ERROR-LITERAL-INDEX TO 1.
           PERFORM 2241-WRITE-SUM1-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
           PERFORM 2242-WRITE-SUM2-TOTALS.
           PERFORM 2243-WRITE-SUM3-TOTALS.
      *
      *
       2241-WRITE-SUM1-TOTALS.
           MOVE SUM1-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID1
                   IN REPORT-TOTALS-RECORD1.
           MOVE EDIT-ERROR-LITERAL-2
                   IN EDIT-ERROR-LITERAL-ENTRY
                   (EDIT-ERROR-LITERAL-INDEX)
                   TO DED-NUMBER.
           MOVE COLLEGE-ELEMENT-TOTAL
                   IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                   (COLLEGE-ELEMENT-ROW-INDEX 1)
                   TO EXCEPT-COUNT
                   IN REPORT-TOTALS-RECORD1.
           MOVE COLLEGE-ELEMENT-TOTAL
                   IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                   (COLLEGE-ELEMENT-ROW-INDEX 2)
                   TO UNKNOWN-COUNT
                   IN REPORT-TOTALS-RECORD1.
           MOVE COLLEGE-ELEMENT-TOTAL
                   IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                   (COLLEGE-ELEMENT-ROW-INDEX 3)
                   TO REASON-COUNT
                   IN REPORT-TOTALS-RECORD1.
           MOVE COLLEGE-ELEMENT-TOTAL
                   IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                   (COLLEGE-ELEMENT-ROW-INDEX 4)
                   TO GRP3-COUNT
                   IN REPORT-TOTALS-RECORD1.
           WRITE REPORT-TOTALS-RECORD1.
           SET ORIGINAL-INDEX-VALUE
                   TO COLLEGE-ELEMENT-ROW-INDEX.
           SEARCH COLLEGE-ELEMENT-TOTALS-ROWS
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO COLLEGE-ELEMENT-ROW-INDEX
               NEXT SENTENCE.
           SET EDIT-ERROR-LITERAL-INDEX
                   TO COLLEGE-ELEMENT-ROW-INDEX.
      *
      *
       2242-WRITE-SUM2-TOTALS.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '01' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-1-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '02' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-2-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '03' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-3-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '04' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-4-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '05' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-5-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '06' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-6-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '07' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-7-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '08' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-8-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '09' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-9-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '10' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-10-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '11' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-11-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '12' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-12-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '13' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-13-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '14' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-14-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '15' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-15-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID2
                   IN REPORT-TOTALS-RECORD2.
           MOVE '16' TO INTEGRITY-ERROR-CODE.
           MOVE CLG-INTEGRITY-ERR-16-TOTAL TO INTEGRITY-ERROR-COUNT.
           WRITE REPORT-TOTALS-RECORD2.
      *    MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
      *    MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
      *    MOVE SUMMARY-PROGRAM-NAME
      *            TO TOTALS-ID2
      *            IN REPORT-TOTALS-RECORD2.
      *    MOVE '17' TO INTEGRITY-ERROR-CODE.
      *    MOVE CLG-INTEGRITY-ERR-17-TOTAL TO INTEGRITY-ERROR-COUNT.
      *    WRITE REPORT-TOTALS-RECORD2.
      *
      *
       2243-WRITE-SUM3-TOTALS.
           MOVE SUM3-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID  TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   TO TOTALS-ID3
                   IN REPORT-TOTALS-RECORD3.
           MOVE TOTAL-COLLEGE-RECORDS-READ
                   TO READ-TOTALS
                   IN REPORT-TOTALS-RECORD3.
           MOVE TOTAL-COLLEGE-RECORDS-REJECT
                   TO REJECTED-TOTALS
                   IN REPORT-TOTALS-RECORD3.
           ACCEPT DATE-FLAG
                   IN REPORT-TOTALS-RECORD3
                   FROM DATE.
           ACCEPT HOLD-TIME
                   IN PROGRAM-LITERAL-AND-WORK
                   FROM TIME.
           MOVE HH-MM
                   IN HOLD-TIME
                   TO TIME-FLAG
                   IN REPORT-TOTALS-RECORD3.
           WRITE  REPORT-TOTALS-RECORD3.
      *
      *
       2300-VALIDATE-COURSE-DATA.
           PERFORM 2310-VALIDATE-COURSE-TITLE.
           PERFORM 2320-VALIDATE-STATUS-FIELDS.
           PERFORM 2330-VALIDATE-CODE-FIELDS.
           PERFORM 2340-VALIDATE-REPEATABILITY.
           PERFORM 2350-VALIDATE-UNITS.
           PERFORM 2360-VALIDATE-SAME-AS-DEPT.
           PERFORM 2370-VALIDATE-COURSE-PRIOR.
           PERFORM 2400-CHECK-DATA-INTEGRITY.
      *
      *
       2310-VALIDATE-COURSE-TITLE.
      *    IF COURSE-TITLE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (6 4)
      *    ELSE
           IF  COURSE-TITLE
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
               MOVE 1 TO COURSE-TITLE-FLAG
           ELSE
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (6 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (6 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2320-VALIDATE-STATUS-FIELDS.
      *    IF  COURSE-CREDIT-STATUS
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (8 4)
      *    ELSE
           IF  NOT COURSE-CREDIT-STATUS-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (8 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (8 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-TRANSF-STATUS
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (9 4)
      *    ELSE
           IF  NOT COURSE-TRANSF-STATUS-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (9 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (9 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-BASIC-SKILLS-STATUS
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (12 4)
      *    ELSE
           IF  NOT VALID-BASIC-SKILLS-STATUS
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (12 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (12 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-COOP-ED-STATUS
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (14 4)
      *    ELSE
           IF  NOT COURSE-COOP-ED-STATUS-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (14 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (14 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-SPECIAL-CLASS-STATUS
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (17 4)
      *    ELSE
           IF  NOT COURSE-SPECIAL-CLASS-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (17 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (17 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2330-VALIDATE-CODE-FIELDS.
      *    IF  COURSE-PROGRAM-CODE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (7 4)
      *    ELSE
           IF  COURSE-PROGRAM-CODE
                   IN COURSE-INVENTORY-RECORD
                   IS NUMERIC
                   AND COURSE-PROGRAM-CODE
                   IN COURSE-INVENTORY-RECORD
                   IS GREATER THAN ZEROS
               MOVE 1 TO COURSE-PROGRAM-CODE-FLAG
               PERFORM 2335-CHECK-PROGRAM-CODE
               PERFORM 2336-TOP-CODE-CHECK
           ELSE
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (7 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (7 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-SAM-PRIORITY-CODE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (13 4)
      *    ELSE
           IF  NOT COURSE-SAM-PRIORITY-CODE-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (13 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (13 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-CLASSIFICATION-CODE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (15 4)
      *    ELSE
           IF  NOT COURSE-CLASSIFICATION-CD-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (15 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (15 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-CAN-CODE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (18 4)
      *        MOVE 1 TO COURSE-CAN-CODE-FLAG
      *    ELSE
           IF  COURSE-CAN-CODE-FIRST
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (18 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (18 EDIT-ERROR-COLUMN-INDEX)
               MOVE 0 TO COURSE-CAN-CODE-FLAG
               ELSE
                    MOVE 1 TO COURSE-CAN-CODE-FLAG.
      *
      *    IF  COURSE-CAN-SEQ-CODE
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (19 4)
      *             MOVE 1 TO COURSE-CAN-SEQ-CODE-FLAG
      *    ELSE
           IF  COURSE-CAN-SEQ-CODE-FIRST
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (19 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (19 EDIT-ERROR-COLUMN-INDEX)
               MOVE 0 TO COURSE-CAN-SEQ-CODE-FLAG
               ELSE
                    MOVE 1 TO COURSE-CAN-SEQ-CODE-FLAG.
      *
      *    IF  COURSE-CROSSWALK-CRS-NAME
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (23 4)
      *            MOVE 1 TO COURSE-CRS-NAME-FLAG
      *    ELSE
           IF  COURSE-CROSSWALK-NAME-FIRST
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (23 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (23 EDIT-ERROR-COLUMN-INDEX)
               MOVE 0 TO COURSE-CRS-NAME-FLAG
               ELSE
                    MOVE 1 TO COURSE-CRS-NAME-FLAG.
      *
      *    IF  COURSE-CROSSWALK-CRS-NUMBER
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (24 4)
      *             MOVE 1 TO COURSE-CRS-NUMBR-FLAG
      *    ELSE
           IF  COURSE-CROSSWALK-NUM-FIRST
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (24 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (24 EDIT-ERROR-COLUMN-INDEX)
               MOVE 0 TO COURSE-CRS-NUMBR-FLAG
               ELSE
                    MOVE 1 TO COURSE-CRS-NUMBR-FLAG.
      *
      *
       2335-CHECK-PROGRAM-CODE.
           IF  TOP-CODE-FLAG = '49'
                   NEXT SENTENCE
           ELSE
                   MOVE '0' TO TOP-CODE-LITERAL.
      *
      *
       2336-TOP-CODE-CHECK.
           SEARCH ALL TOP-CODE-TBL
               WHEN TOP-CODE-Y (TOPIDX)
                   IS EQUAL TO COURSE-PROGRAM-CODE
                       IN COURSE-INVENTORY-RECORD
               MOVE 1 TO TOP-CODE-VALID-FLAG.
               IF NOT TOP-CODE-VALID
                   ADD 1 TO COLLEGE-ELEMENT-TOTAL
                        IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                        (7 1)
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE VALIDITY-LITERAL
                        TO VALIDATION-FLAG
                        IN EDIT-ERROR-TABLE-COLUMNS
                        (7 EDIT-ERROR-COLUMN-INDEX)
                   MOVE 0 TO TOP-CODE-VALID-FLAG.
      *
      *
       2340-VALIDATE-REPEATABILITY.
      *    IF  COURSE-REPEATABILITY
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (16 4)
      *    ELSE
           IF  NOT COURSE-REPEATABILITY-VALID
                   IN COURSE-INVENTORY-RECORD
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (16 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (16 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2350-VALIDATE-UNITS.
      *    IF  COURSE-UNITS-MAXIMUM
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (10 4)
      *    ELSE
           IF  COURSE-UNITS-MAXIMUM
                   IN COURSE-INVENTORY-RECORD
                   IS NUMERIC
               MOVE 1 TO COURSE-UNITS-MAXIMUM-FLAG
           ELSE
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (10 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (10 EDIT-ERROR-COLUMN-INDEX).
      *
      *    IF  COURSE-UNITS-MINIMUM
      *            IN COURSE-INVENTORY-RECORD
      *            IS EQUAL TO SPACES
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (11 4)
      *    ELSE
           IF  COURSE-UNITS-MINIMUM
                   IN COURSE-INVENTORY-RECORD
                   IS NUMERIC
               MOVE 1 TO COURSE-UNITS-MINIMUM-FLAG
           ELSE
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (11 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (11 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2360-VALIDATE-SAME-AS-DEPT.
           IF COURSE-SAME-AS-DEPTNO1
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (20 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (20 EDIT-ERROR-COLUMN-INDEX).
           IF COURSE-SAME-AS-DEPTNO2
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (21 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (21 EDIT-ERROR-COLUMN-INDEX).
           IF COURSE-SAME-AS-DEPTNO3
                   IN COURSE-INVENTORY-RECORD
                   IS EQUAL TO SPACES
               ADD 1 TO COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (22 1)
               MOVE 1 TO REJECT-RECORD-FLAG
               MOVE VALIDITY-LITERAL
                       TO VALIDATION-FLAG
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (22 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2370-VALIDATE-COURSE-PRIOR.
           IF COURSE-PRIOR-VALID-CODES
                MOVE 1 TO COURSE-PRIOR-LEVEL-FLAG
           ELSE
               IF NOT COURSE-PRIOR-VALID-CODES
                   ADD 1 TO COLLEGE-ELEMENT-TOTAL
                           IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                           (25 1)
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE VALIDITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                       (    25 EDIT-ERROR-COLUMN-INDEX).
      *
      *
       2400-CHECK-DATA-INTEGRITY.
      *
      * CHECK CREDIT AND TRANSFER STATUS INTEGRITY.
      *
           IF  COURSE-CREDIT-STATUS-VALID
                   AND COURSE-TRANSF-STATUS-VALID
                   AND (COURSE-CREDIT-STATUS
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
                   AND COURSE-TRANSF-STATUS
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES)
               IF  (TRANSFER-STATUS-A-OR-B
                       AND NOT CREDIT-STATUS-D)
                   ADD 1 TO CLG-INTEGRITY-ERR-1-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (8 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK CREDIT STATUS WITH UNITS OF CREDIT MAXIMUM.
      *
           IF  COURSE-CREDIT-STATUS-VALID
                   AND COURSE-UNITS-MAXIMUM-VALID
                   AND (COURSE-CREDIT-STATUS
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
                   AND COURSE-UNITS-MAXIMUM
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES)
               IF  (CREDIT-STATUS-N
                       AND COURSE-UNITS-MAXIMUM
                       IN COURSE-INVENTORY-RECORD
                       IS NOT EQUAL TO ZERO)
                   ADD 1 TO CLG-INTEGRITY-ERR-2-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (8 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (10 EDIT-ERROR-COLUMN-INDEX).
           IF  COURSE-CREDIT-STATUS-VALID
                   AND COURSE-UNITS-MINIMUM-VALID
                   AND (COURSE-CREDIT-STATUS
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
                   AND COURSE-UNITS-MINIMUM
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES)
               IF  (CREDIT-STATUS-N
                       AND COURSE-UNITS-MINIMUM
                       IN COURSE-INVENTORY-RECORD
                       IS NOT EQUAL TO ZERO)
                   ADD 1 TO CLG-INTEGRITY-ERR-3-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (8 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (11 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK INTEGRITY BETWEEN BASIC SKILLS STATUS AND CREDIT STATUS.
      *
           IF  VALID-BASIC-SKILLS-STATUS
                   AND COURSE-CREDIT-STATUS-VALID
               IF  (BASIC-SKILLS-STATUS-P
                       AND NOT CREDIT-STATUS-C)
                   ADD 1 TO CLG-INTEGRITY-ERR-4-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (8 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (12 EDIT-ERROR-COLUMN-INDEX)
               ELSE
               IF  (BASIC-SKILLS-STATUS-B
                       AND NOT CREDIT-STATUS-C-OR-N)
                   ADD 1 TO CLG-INTEGRITY-ERR-5-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (8 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (12 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK INTEGRITY BETWEEN CAN CODE AND CAN SEQ CODE.
      *
           IF COURSE-CAN-CODE-VALID AND COURSE-CAN-SEQ-CODE-VALID
           IF  COURSE-CAN-CODE
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
              AND COURSE-CAN-SEQ-CODE
                   IN COURSE-INVENTORY-RECORD
                   IS NOT EQUAL TO SPACES
           IF COURSE-CAN-CODE-X
                AND (COURSE-CAN-SEQ-CODE-X OR COURSE-CAN-SEQ-CODE-Y)
                   ADD 1 TO CLG-INTEGRITY-ERR-6-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (18 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (19 EDIT-ERROR-COLUMN-INDEX)
           ELSE
               IF COURSE-CAN-CODE-Y
                   AND NOT COURSE-CAN-SEQ-CODE-Y
                      ADD 1 TO CLG-INTEGRITY-ERR-7-TOTAL
                      MOVE 1 TO REJECT-RECORD-FLAG
                      MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (18 EDIT-ERROR-COLUMN-INDEX)
                      MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (19 EDIT-ERROR-COLUMN-INDEX)
               ELSE
               IF NOT COURSE-CAN-CODE-X AND NOT COURSE-CAN-CODE-Y
                   AND COURSE-CAN-SEQ-CODE-Y
                      ADD 1 TO CLG-INTEGRITY-ERR-8-TOTAL
                      MOVE 1 TO REJECT-RECORD-FLAG
                      MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (18 EDIT-ERROR-COLUMN-INDEX)
                      MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (19 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK INTEGRITY SAME AS DEPARTMENT NUMBER AND COURSE ID.
      *
           IF COURSE-SAME-AS-1-VALID AND COURSE-ID-VALID
           IF COURSE-SAME-AS-DEPTNO1
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL SPACES
               AND COURSE-DEPARTMENT-NUMBER
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL TO SPACES
           IF COURSE-SAME-AS-DEPTNO1
                   IN COURSE-INVENTORY-RECORD
                   = COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   ADD 1 TO CLG-INTEGRITY-ERR-9-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (2 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (20 EDIT-ERROR-COLUMN-INDEX).
           IF COURSE-SAME-AS-2-VALID AND COURSE-ID-VALID
           IF COURSE-SAME-AS-DEPTNO2
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL SPACES
               AND COURSE-DEPARTMENT-NUMBER
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL TO SPACES
           IF COURSE-SAME-AS-DEPTNO2
                   IN COURSE-INVENTORY-RECORD
                   = COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   ADD 1 TO CLG-INTEGRITY-ERR-10-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (2 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (21 EDIT-ERROR-COLUMN-INDEX).
           IF COURSE-SAME-AS-3-VALID AND COURSE-ID-VALID
           IF COURSE-SAME-AS-DEPTNO3
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL SPACES
               AND COURSE-DEPARTMENT-NUMBER
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL TO SPACES
           IF COURSE-SAME-AS-DEPTNO3
                   IN COURSE-INVENTORY-RECORD
                   = COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   ADD 1 TO CLG-INTEGRITY-ERR-11-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (2 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (22 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK CRS NAME WITH TRANSFER STATUS.
      *
           IF COURSE-TRANSF-STATUS-VALID AND COURSE-CRS-NAME-VALID
           IF COURSE-TRANSF-STATUS
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL SPACES
               AND COURSE-CROSSWALK-CRS-NAME
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL TO SPACES
           IF  TRANSFER-STATUS-A-OR-B
                   AND COURSE-CRS-NAME-Y
                   ADD 1 TO CLG-INTEGRITY-ERR-12-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (23 EDIT-ERROR-COLUMN-INDEX)
           ELSE
           IF  TRANSFER-STATUS-C
                   AND NOT COURSE-CRS-NAME-Y
                   ADD 1 TO CLG-INTEGRITY-ERR-13-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (23 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK CRS NUMBER WITH TRANSFER STATUS.
      *
           IF COURSE-TRANSF-STATUS-VALID AND COURSE-CRS-NUMBR-VALID
           IF COURSE-TRANSF-STATUS
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL SPACES
               AND COURSE-CROSSWALK-CRS-NUMBER
               IN COURSE-INVENTORY-RECORD
               NOT EQUAL TO SPACES
           IF  TRANSFER-STATUS-A-OR-B
                   AND COURSE-CRS-NUM-Y
                   ADD 1 TO CLG-INTEGRITY-ERR-14-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (24 EDIT-ERROR-COLUMN-INDEX)
           ELSE
           IF  TRANSFER-STATUS-C
                   AND NOT COURSE-CRS-NUM-Y
                   ADD 1 TO CLG-INTEGRITY-ERR-15-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (24 EDIT-ERROR-COLUMN-INDEX).
      *
      * CHECK COURSE PRIOR LEVEL WITH COURSE TRANSFER STATUS.
      *
           IF COURSE-PRIOR-VALID
              AND NOT COURSE-PRIOR-NOT-APP
              IF NOT TRANSFER-STATUS-C
                   ADD 1 TO CLG-INTEGRITY-ERR-16-TOTAL
                   MOVE 1 TO REJECT-RECORD-FLAG
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (25 EDIT-ERROR-COLUMN-INDEX)
                   MOVE INTEGRITY-LITERAL
                           TO VALIDATION-FLAG
                           IN EDIT-ERROR-TABLE-COLUMNS
                           (9 EDIT-ERROR-COLUMN-INDEX).
      *
      *
      * CHECK CRS NAME WITH CRS NUMBER.
      *
      *    IF COURSE-CRS-NAME-VALID AND COURSE-CRS-NUMBR-VALID
      *    IF  COURSE-CRS-NAME-Y
      *            AND NOT COURSE-CRS-NUM-Y
      *            ADD 1 TO CLG-INTEGRITY-ERR-14-TOTAL
      *            MOVE 1 TO REJECT-RECORD-FLAG
      *            MOVE INTEGRITY-LITERAL
      *                    TO VALIDATION-FLAG
      *                    IN EDIT-ERROR-TABLE-COLUMNS
      *                    (23 EDIT-ERROR-COLUMN-INDEX)
      *            MOVE INTEGRITY-LITERAL
      *                    TO VALIDATION-FLAG
      *                    IN EDIT-ERROR-TABLE-COLUMNS
      *                    (24 EDIT-ERROR-COLUMN-INDEX)
      *    ELSE
      *    IF  NOT COURSE-CRS-NAME-Y
      *            AND COURSE-CRS-NUM-Y
      *            ADD 1 TO CLG-INTEGRITY-ERR-15-TOTAL
      *            MOVE 1 TO REJECT-RECORD-FLAG
      *            MOVE INTEGRITY-LITERAL
      *                    TO VALIDATION-FLAG
      *                    IN EDIT-ERROR-TABLE-COLUMNS
      *                    (23 EDIT-ERROR-COLUMN-INDEX)
      *            MOVE INTEGRITY-LITERAL
      *                    TO VALIDATION-FLAG
      *                    IN EDIT-ERROR-TABLE-COLUMNS
      *                    (24 EDIT-ERROR-COLUMN-INDEX).
      *
      *
      *2500-ACCUMULATE-UNKNOWNS.
      *    IF  COURSE-SAM-PRIORITY-CODE-UNKN
      *        ADD 1 TO COLLEGE-ELEMENT-TOTAL
      *                IN COLLEGE-ELEMENT-TOTALS-COLUMNS
      *                (13 2).
      *
      *
       2600-FILL-ERROR-MATRIX.
           MOVE COURSE-PERM-DIST-ID
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (1 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (2 EDIT-ERROR-COLUMN-INDEX).
           MOVE RECORD-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (3 EDIT-ERROR-COLUMN-INDEX).
           MOVE COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (4 EDIT-ERROR-COLUMN-INDEX).
           MOVE TERM-IDENTIFIER
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (5 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-TITLE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (6 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-PROGRAM-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (7 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CREDIT-STATUS
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (8 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-TRANSF-STATUS
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (9 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-UNITS-MAXIMUM
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (10 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-UNITS-MINIMUM
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (11 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-BASIC-SKILLS-STATUS
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (12 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-SAM-PRIORITY-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (13 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-COOP-ED-STATUS
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (14 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CLASSIFICATION-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (15 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-REPEATABILITY
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (16 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-SPECIAL-CLASS-STATUS
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (17 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CAN-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (18 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CAN-SEQ-CODE
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (19 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-SAME-AS-DEPTNO1
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (20 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-SAME-AS-DEPTNO2
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (21 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-SAME-AS-DEPTNO3
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (22 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CROSSWALK-CRS-NAME
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (23 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-CROSSWALK-CRS-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (24 EDIT-ERROR-COLUMN-INDEX).
           MOVE COURSE-PRIOR-TO-COLLEGE-LEVEL
                   IN COURSE-INVENTORY-RECORD
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (25 EDIT-ERROR-COLUMN-INDEX).
           IF REJECT-RECORD
               MOVE REJECTED-MESSAGE
                       TO DATA-ELEMENT
                       IN EDIT-ERROR-TABLE-COLUMNS
                       (26 EDIT-ERROR-COLUMN-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO EDIT-ERROR-COLUMN-INDEX.
           SEARCH EDIT-ERROR-TABLE-COLUMNS
           AT END
               PERFORM 8400-PRINT-ERROR-REPORT
               SET EDIT-ERROR-COLUMN-INDEX TO 1
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO EDIT-ERROR-COLUMN-INDEX
               NEXT SENTENCE.
      *
      *
       8000-READ-COURSE-INV-DATA.
           READ COURSE-INV-DATA-FILE
           AT END
               MOVE 1 TO END-OF-FILE-FLAG.
      *
      *
       8200-PRINT-COLLEGE-TOTALS.
           MOVE 2 TO HEADER-CONTROL-FLAG.
           MOVE SUM1-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET COLLEGE-ELEMENT-ROW-INDEX TO 1.
           SET EDIT-ERROR-LITERAL-INDEX TO 1.
           PERFORM 8210-PRINT-CLG-ELEMENT-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
           PERFORM 8220-PRINT-CLG-INTGRTY-TOTALS.
           MOVE 6 TO HEADER-CONTROL-FLAG.
           MOVE SUM3-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           WRITE PRINT-RECORD-2
                   FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET TOTAL-COLLEGE-INDEX TO 1.
           SET TOTAL-DESCRIPTION-INDEX TO 1.
           PERFORM 8230-PRINT-COLLEGE-SUM-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
      *
      *
       8210-PRINT-CLG-ELEMENT-TOTALS.
           MOVE EDIT-ERROR-LITERAL-AREA
                   IN EDIT-ERROR-LITERAL-ENTRY
                   (EDIT-ERROR-LITERAL-INDEX)
                   TO ELEMENT-LITERAL-AREA.
           SET COLLEGE-ELEMENT-COLUMN-INDEX TO 1.
           SET ELEMENT-COLUMN-INDEX TO 1.
           MOVE ZERO TO PRINT-LINE-FLAG.
           PERFORM 8211-FILL-CLG-PRINT-LINE
                   UNTIL PRINT-LINE-FILLED.
           WRITE PRINT-RECORD-2
                   FROM ELEMENT-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO ELEMENT-TOTAL-DETAIL-LINE.
           SET ORIGINAL-INDEX-VALUE
                   TO COLLEGE-ELEMENT-ROW-INDEX.
           SEARCH COLLEGE-ELEMENT-TOTALS-ROWS
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO COLLEGE-ELEMENT-ROW-INDEX
               NEXT SENTENCE.
           SET EDIT-ERROR-LITERAL-INDEX
                   TO COLLEGE-ELEMENT-ROW-INDEX.
      *
      *
       8211-FILL-CLG-PRINT-LINE.
           SET  ELEMENT-TABLE-ROW-ID
                   TO COLLEGE-ELEMENT-ROW-INDEX.
           SET  ELEMENT-TABLE-COLUMN-ID
                   TO COLLEGE-ELEMENT-COLUMN-INDEX.
           IF (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-2)
                       OR
                  (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-3)
                       OR
                  (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-4)
      *                OR
      *           (ELEMENT-ROWS-13
      *            AND ELEMENT-COLUMN-4)
      *                OR
      *           (ELEMENT-ROWS-17
      *            AND ELEMENT-COLUMN-4)
               MOVE NOT-APPLICABLE-LITERAL
                       TO ELEMENT-N-A
                       IN ELEMENT-COLUMNS
                       (ELEMENT-COLUMN-INDEX)
           ELSE
               MOVE COLLEGE-ELEMENT-TOTAL
                       IN COLLEGE-ELEMENT-TOTALS-COLUMNS
                       (COLLEGE-ELEMENT-ROW-INDEX
                        COLLEGE-ELEMENT-COLUMN-INDEX)
                       TO ELEMENT-TOTAL
                       IN ELEMENT-COLUMNS
                       (ELEMENT-COLUMN-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO COLLEGE-ELEMENT-COLUMN-INDEX.
           SEARCH COLLEGE-ELEMENT-TOTALS-COLUMNS
           AT END
               MOVE 1 TO PRINT-LINE-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO COLLEGE-ELEMENT-COLUMN-INDEX
               NEXT SENTENCE.
           SET ELEMENT-COLUMN-INDEX
                   TO COLLEGE-ELEMENT-COLUMN-INDEX.
      *
      *
       8220-PRINT-CLG-INTGRTY-TOTALS.
           MOVE 3 TO HEADER-CONTROL-FLAG.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE HOLD-COLLEGE-ID TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-1-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-1-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-2-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-2-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-3-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-3-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-4-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-4-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-5-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-5-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-6-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-6-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-7-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-7-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-8-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-8-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-9-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-9-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-10-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-10-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-11-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-11-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-12-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-12-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-13-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-13-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-14-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-14-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-15-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-15-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE CLG-INTEGRITY-ERR-16-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-16-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
      *    MOVE SPACES TO PRINT-RECORD-2.
      *    MOVE CLG-INTEGRITY-ERR-17-TOTAL TO INTEGRITY-TOTAL.
      *    MOVE INTEGRITY-ERR-17-DESC TO INTEGRITY-DESCRIPTION.
      *    WRITE PRINT-RECORD-2
      *            FROM INTEGRITY-TOTAL-DETAIL-LINE
      *            AFTER ADVANCING 2 LINES.
      *
      *
       8230-PRINT-COLLEGE-SUM-TOTALS.
           MOVE TOTAL-DESCRIPTION
                   IN TOTAL-DESCRIPTION-ENTRY
                   (TOTAL-DESCRIPTION-INDEX)
                   TO TOTAL-DESCRIPTION
                   IN TOTAL-DETAIL-LINE.
           MOVE COLLEGE-TOTAL
                   IN TOTAL-COLLEGE-ENTRY
                   (TOTAL-COLLEGE-INDEX)
                   TO TOTAL-FIELD
                   IN TOTAL-DETAIL-LINE.
           WRITE PRINT-RECORD-2
                   FROM TOTAL-DETAIL-LINE
                   AFTER ADVANCING 1 LINE.
           SET ORIGINAL-INDEX-VALUE
                   TO TOTAL-COLLEGE-INDEX.
           SEARCH TOTAL-COLLEGE-ENTRY
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO TOTAL-COLLEGE-INDEX
               NEXT SENTENCE.
           SET TOTAL-DESCRIPTION-INDEX
                   TO TOTAL-COLLEGE-INDEX.
      *
      *
       8400-PRINT-ERROR-REPORT.
           IF  PAGE-FULL
               MOVE ZERO TO HEADER-CONTROL-FLAG
               MOVE DETAIL-PROGRAM-NAME
                       IN PROGRAM-LITERAL-AND-WORK
                       TO PROGRAM-NAME
                       IN HEADER-REPORT-ID
               PERFORM 8500-PRINT-HEADERS
               MOVE ZERO TO PAGE-FULL-FLAG.
           ADD 1 TO PAGE-FULL-FLAG.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET EDIT-ERROR-ROW-INDEX TO 1.
           SET EDIT-ERROR-LITERAL-INDEX TO 1.
           PERFORM 8410-PRINT-DETAIL-LINES
                   UNTIL TABLE-SEARCH-COMPLETED.
           MOVE SPACES TO EDIT-ERROR-TABLE.
           SET EDIT-ERROR-COLUMN-INDEX TO 1.
      *
      *
       8410-PRINT-DETAIL-LINES.
           MOVE EDIT-ERROR-LITERAL-AREA
                   IN EDIT-ERROR-LITERAL-ENTRY
                   (EDIT-ERROR-LITERAL-INDEX)
                   TO DETAIL-LITERAL-AREA.
           SET EDIT-ERROR-COLUMN-INDEX TO 1.
           SET DETAIL-COLUMN-INDEX TO 1.
           MOVE ZERO TO PRINT-LINE-FLAG.
           PERFORM 8430-FILL-PRINT-LINE
                   UNTIL PRINT-LINE-FILLED
           WRITE PRINT-RECORD
                   FROM EDIT-ERROR-DETAIL-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO EDIT-ERROR-DETAIL-LINE.
           SET ORIGINAL-INDEX-VALUE
                   TO EDIT-ERROR-ROW-INDEX.
           SEARCH EDIT-ERROR-TABLE-ROWS
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO EDIT-ERROR-ROW-INDEX
               SET EDIT-ERROR-LITERAL-INDEX
                       TO EDIT-ERROR-ROW-INDEX.
      *
      *
       8430-FILL-PRINT-LINE.
           MOVE DATA-ELEMENT
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (EDIT-ERROR-ROW-INDEX
                    EDIT-ERROR-COLUMN-INDEX)
                   TO DATA-ELEMENT
                   IN EDIT-ERROR-DETAIL-LINE
                   (DETAIL-COLUMN-INDEX).
           MOVE VALIDATION-FLAG
                   IN EDIT-ERROR-TABLE-COLUMNS
                   (EDIT-ERROR-ROW-INDEX
                    EDIT-ERROR-COLUMN-INDEX)
                   TO VALIDATION-FLAG
                   IN EDIT-ERROR-DETAIL-LINE
                   (DETAIL-COLUMN-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO EDIT-ERROR-COLUMN-INDEX.
           SEARCH EDIT-ERROR-TABLE-COLUMNS
           AT END
               MOVE 1 TO PRINT-LINE-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO EDIT-ERROR-COLUMN-INDEX
               SET DETAIL-COLUMN-INDEX
                       TO EDIT-ERROR-COLUMN-INDEX.
      *
      *
       8500-PRINT-HEADERS.
           MOVE SPACES TO PRINT-RECORD.
           MOVE ZEROS TO REPORT-LINE-COUNTER.
           ADD 1 TO REPORT-PAGE-COUNTER.
           MOVE REPORT-PAGE-COUNTER
                   TO HEADER-PAGE-NUMBER.
           IF FIRST-PAGE
               PERFORM 1HDR-FIRST-PAGE-ROUTINE.
           IF NARROW-PAPER-REQUIRED
               PERFORM 2HDR-NARROW-PAPER-ROUTINE
           ELSE
               PERFORM 3HDR-WIDE-PAPER-ROUTINE.
           PERFORM 7HDR-USER-SUPPLIED-HEADINGS.
      *
      *
       1HDR-FIRST-PAGE-ROUTINE.
           ACCEPT REPORT-DATE
                   IN HEADER-HOLDING-AREA
                   FROM DATE.
           ACCEPT REPORT-TIME
                   IN HEADER-HOLDING-AREA
                   FROM TIME.
           MOVE FULL-MONTH-NAME
                   IN HEADER-MONTH-TABLE
                   (REPORT-MONTH)
                   TO HEADER-MONTH.
           MOVE REPORT-DAY
                   IN HEADER-HOLDING-AREA
                   TO HEADER-DAY
                   IN HEADER-STATISTICS.
           MOVE REPORT-YEAR
                   IN HEADER-HOLDING-AREA
                   TO HEADER-YEAR
                   IN HEADER-STATISTICS.
           MOVE ACTUAL-CLOCK-TIME
                   IN HEADER-HOLDING-AREA
                   TO HEADER-TIME
                   IN HEADER-STATISTICS.
           MOVE ZERO TO FIRST-PAGE-SWITCH.
           INSPECT HEADER-TIME-AREA
                   REPLACING ALL '/'
                   BY ':'.
      *
      *
       2HDR-NARROW-PAPER-ROUTINE.
           MOVE COMMUNITY-COLLEGE-TITLE
                   IN HEADER-HOLDING-AREA
                   TO HEADING-TITLE-LINE.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING PAGE.
           MOVE SPACES TO HEADING-LINE.
           MOVE MIS-PILOT-TITLE
                   IN HEADER-HOLDING-AREA
                   TO HEADING-TITLE-LINE.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-TIME
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-PAGE
                   TO NARROW-RIGHT-STATISTIC.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-ID
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-DATE
                   TO NARROW-RIGHT-STATISTIC.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
      *
      *
       3HDR-WIDE-PAPER-ROUTINE.
           MOVE COMMUNITY-COLLEGE-TITLE
                   IN HEADER-HOLDING-AREA
                   TO WIDE-HEADING-LOCATION
                   IN HEADING-TITLE-LINE.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING PAGE.
           MOVE SPACES TO HEADING-LINE.
           MOVE MIS-PILOT-TITLE
                   IN HEADER-HOLDING-AREA
                   TO WIDE-HEADING-LOCATION
                   IN HEADING-TITLE-LINE.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-TIME
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-PAGE
                   TO WIDE-RIGHT-STATISTIC.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-ID
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-DATE
                   TO WIDE-RIGHT-STATISTIC.
           WRITE PRINT-RECORD
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
      *
      *
       7HDR-USER-SUPPLIED-HEADINGS.
           WRITE PRINT-RECORD
                   FROM REPORT-TITLE-LINE-1
                   AFTER ADVANCING 1 LINE.
           WRITE PRINT-RECORD
                   FROM REPORT-TITLE-LINE-2
                   AFTER ADVANCING 1 LINES.
           WRITE PRINT-RECORD
                   FROM REPORT-TITLE-LINE-3
                   AFTER ADVANCING 2 LINES.
           WRITE PRINT-RECORD
                   FROM REPORT-TITLE-LINE-11
                   AFTER ADVANCING 2 LINES.
           IF  KILL-PROCESS
               WRITE PRINT-RECORD
                       FROM REPORT-TITLE-LINE-6
                       AFTER ADVANCING 2 LINES
               WRITE PRINT-RECORD
                       FROM KILL-PROCESS-COLUMN-LINE-1
                       AFTER ADVANCING 2 LINES
           ELSE
                WRITE PRINT-RECORD
                       FROM REPORT-COLUMN-LINE-1
                       AFTER ADVANCING 2 LINES.
      *
      *
       85002-PRINT-HEADERS.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE ZEROS TO REPORT-LINE-COUNTER.
           ADD 1 TO REPORT-PAGE-COUNTER.
           MOVE REPORT-PAGE-COUNTER
                   TO HEADER-PAGE-NUMBER.
           IF FIRST-PAGE
               PERFORM 1HDR-FIRST-PAGE-ROUTINE-2.
           IF NARROW-PAPER-REQUIRED
               PERFORM 2HDR-NARROW-PAPER-ROUTINE-2
           ELSE
               PERFORM 3HDR-WIDE-PAPER-ROUTINE-2.
           PERFORM 7HDR-USER-SUPPLIED-HEADINGS-2.
      *
      *
       1HDR-FIRST-PAGE-ROUTINE-2.
           ACCEPT REPORT-DATE
                   IN HEADER-HOLDING-AREA
                   FROM DATE.
           ACCEPT REPORT-TIME
                   IN HEADER-HOLDING-AREA
                   FROM TIME.
           MOVE FULL-MONTH-NAME
                   IN HEADER-MONTH-TABLE
                   (REPORT-MONTH)
                   TO HEADER-MONTH.
           MOVE REPORT-DAY
                   IN HEADER-HOLDING-AREA
                   TO HEADER-DAY
                   IN HEADER-STATISTICS.
           MOVE REPORT-YEAR
                   IN HEADER-HOLDING-AREA
                   TO HEADER-YEAR
                   IN HEADER-STATISTICS.
           MOVE ACTUAL-CLOCK-TIME
                   IN HEADER-HOLDING-AREA
                   TO HEADER-TIME
                   IN HEADER-STATISTICS.
           MOVE ZERO TO FIRST-PAGE-SWITCH.
           INSPECT HEADER-TIME-AREA
                   REPLACING ALL '/'
                   BY ':'.
      *
      *
       2HDR-NARROW-PAPER-ROUTINE-2.
           MOVE COMMUNITY-COLLEGE-TITLE
                   IN HEADER-HOLDING-AREA
                   TO HEADING-TITLE-LINE.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING PAGE.
           MOVE SPACES TO HEADING-LINE.
           MOVE MIS-PILOT-TITLE
                   IN HEADER-HOLDING-AREA
                   TO HEADING-TITLE-LINE.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-TIME
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-PAGE
                   TO NARROW-RIGHT-STATISTIC.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-ID
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-DATE
                   TO NARROW-RIGHT-STATISTIC.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
      *
      *
       3HDR-WIDE-PAPER-ROUTINE-2.
           MOVE COMMUNITY-COLLEGE-TITLE
                   IN HEADER-HOLDING-AREA
                   TO WIDE-HEADING-LOCATION
                   IN HEADING-TITLE-LINE.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING PAGE.
           MOVE SPACES TO HEADING-LINE.
           MOVE MIS-PILOT-TITLE
                   IN HEADER-HOLDING-AREA
                   TO WIDE-HEADING-LOCATION
                   IN HEADING-TITLE-LINE.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-TIME
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-PAGE
                   TO WIDE-RIGHT-STATISTIC.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
           MOVE HEADER-REPORT-ID
                   TO LEFT-STATISTIC.
           MOVE HEADER-REPORT-DATE
                   TO WIDE-RIGHT-STATISTIC.
           WRITE PRINT-RECORD-2
                   FROM HEADING-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO HEADING-LINE.
      *
      *
       7HDR-USER-SUPPLIED-HEADINGS-2.
           WRITE PRINT-RECORD-2
                   FROM REPORT-TITLE-LINE-1
                   AFTER ADVANCING 1 LINE.
           WRITE PRINT-RECORD-2
                   FROM REPORT-TITLE-LINE-2
                   AFTER ADVANCING 1 LINES.
           WRITE PRINT-RECORD-2
                   FROM REPORT-TITLE-LINE-3
                   AFTER ADVANCING 2 LINES.
           IF  COLLEGE-ELEMENT-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-7
                       AFTER ADVANCING 2 LINES.
           IF  DISTRICT-ELEMENT-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-8
                       AFTER ADVANCING 2 LINES.
           IF  COLLEGE-ELEMENT-TOTALS
                   IN HEADER-CONTROL-FLAG
                   OR DISTRICT-ELEMENT-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM ELEMENT-COLUMN-LINE-1
                       AFTER ADVANCING 2 LINE
               WRITE PRINT-RECORD-2
                       FROM ELEMENT-COLUMN-LINE-2
                       AFTER ADVANCING 1 LINE
               WRITE PRINT-RECORD-2
                       FROM ELEMENT-COLUMN-LINE-3
                       AFTER ADVANCING 1 LINES.
           IF  COLLEGE-INTEGRITY-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-9
                       AFTER ADVANCING 2 LINES.
           IF  DISTRICT-INTEGRITY-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-10
                       AFTER ADVANCING 2 LINES.
           IF  COLLEGE-SUMMARY-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-4
                       AFTER ADVANCING 2 LINES.
           IF  DISTRICT-SUMMARY-TOTALS
                   IN HEADER-CONTROL-FLAG
               WRITE PRINT-RECORD-2
                       FROM REPORT-TITLE-LINE-5
                       AFTER ADVANCING 2 LINES.
      *
      *
       8600-WRITE-COURSE-DATA.
           MOVE SPACES TO EDITED-COURSE-INVENTORY-RECORD.
           PERFORM 8610-MOVE-DATA-TO-OUT-RECORD.
           WRITE EDITED-COURSE-INVENTORY-RECORD.
           ADD 1 TO TOTAL-COLLEGE-RECORDS-WRITTEN.
      *
      *
       8610-MOVE-DATA-TO-OUT-RECORD.
           PERFORM 8611-MOVE-KEY-ELEMENTS.
           PERFORM 8612-MOVE-NON-KEY-ELEMENTS.
      *
      *
       8611-MOVE-KEY-ELEMENTS.
           MOVE COLLEGE-ID
                   IN COURSE-INVENTORY-RECORD
                   TO COLLEGE-ID
                   IN EDITED-COURSE-INVENTORY-RECORD.
           MOVE TERM-IDENTIFIER
                   IN COURSE-INVENTORY-RECORD
                   TO TERM-IDENTIFIER
                   IN EDITED-COURSE-INVENTORY-RECORD.
           MOVE COURSE-PERM-DIST-ID
                   IN COURSE-INVENTORY-RECORD
                   TO COURSE-PERM-DIST-ID
                   IN EDITED-COURSE-INVENTORY-RECORD.
           MOVE COURSE-DEPARTMENT-NUMBER
                   IN COURSE-INVENTORY-RECORD
                   TO COURSE-DEPARTMENT-NUMBER
                   IN EDITED-COURSE-INVENTORY-RECORD.
      *
      *
       8612-MOVE-NON-KEY-ELEMENTS.
               MOVE COURSE-TITLE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-TITLE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-PROGRAM-CODE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-PROGRAM-CODE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CREDIT-STATUS
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CREDIT-STATUS
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-TRANSF-STATUS
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-TRANSF-STATUS
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-UNITS-MAXIMUM
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-UNITS-MAXIMUM
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-UNITS-MINIMUM
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-UNITS-MINIMUM
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-BASIC-SKILLS-STATUS
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-BASIC-SKILLS-STATUS
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-SAM-PRIORITY-CODE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-SAM-PRIORITY-CODE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-COOP-ED-STATUS
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-COOP-ED-STATUS
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CLASSIFICATION-CODE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CLASSIFICATION-CODE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-REPEATABILITY
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-REPEATABILITY
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-SPECIAL-CLASS-STATUS
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-SPECIAL-CLASS-STATUS
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CAN-CODE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CAN-CODE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CAN-SEQ-CODE
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CAN-SEQ-CODE
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-SAME-AS-DEPTNO1
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-SAME-AS-DEPTNO1
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-SAME-AS-DEPTNO2
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-SAME-AS-DEPTNO2
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-SAME-AS-DEPTNO3
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-SAME-AS-DEPTNO3
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CROSSWALK-CRS-NAME
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CROSSWALK-CRS-NAME
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-CROSSWALK-CRS-NUMBER
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CROSSWALK-CRS-NUMBER
                       IN EDITED-COURSE-INVENTORY-RECORD.
               MOVE COURSE-PRIOR-TO-COLLEGE-LEVEL
                       IN COURSE-INVENTORY-RECORD
                       TO COURSE-CROSSWALK-CRS-NUMBER
                       IN EDITED-COURSE-INVENTORY-RECORD.
      *
      *
       8800-PRINT-DISTRICT-TOTALS.
           MOVE DISTRICT-NAME
                   IN DISTRICT-CODE-ENTRY
                   (DISTRICT-CODE-INDEX)
                   TO COLLEGE-NAME
                   IN REPORT-TITLE-LINE-3.
           MOVE 4 TO HEADER-CONTROL-FLAG.
           MOVE SUM1-PROGRAM-LIT TO REPORT-ID-LIT.
           IF  SINGLE-COLLEGE-DISTRICT
                   IN DISTRICT-CODE-ENTRY
                   (DISTRICT-CODE-INDEX)
               MOVE HOLD-COLLEGE-ID TO REPORT-CLG-DIST-ID
           ELSE
               MOVE DISTRICT-ID
                       IN RUNTIME-PARAMETER-AREA
                       TO REPORT-CLG-DIST-ID.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET DISTRICT-ELEMENT-ROW-INDEX TO 1.
           SET EDIT-ERROR-LITERAL-INDEX TO 1.
           PERFORM 8810-PRINT-DIST-ELEMENT-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
           PERFORM 8820-PRINT-DIST-INTGRTY-TOTALS.
           MOVE 7 TO HEADER-CONTROL-FLAG.
           MOVE SUM3-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           WRITE PRINT-RECORD-2
                   FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE ZERO TO TABLE-SEARCH-FLAG.
           SET TOTAL-DISTRICT-INDEX TO 1.
           SET TOTAL-DESCRIPTION-INDEX TO 1.
           PERFORM 8830-PRINT-DISTRICT-SUM-TOTALS
                   UNTIL TABLE-SEARCH-COMPLETED.
      *
      *
       8810-PRINT-DIST-ELEMENT-TOTALS.
           MOVE EDIT-ERROR-LITERAL-AREA
                   IN EDIT-ERROR-LITERAL-ENTRY
                   (EDIT-ERROR-LITERAL-INDEX)
                   TO ELEMENT-LITERAL-AREA.
           SET DISTRICT-ELEMENT-COLUMN-INDEX TO 1.
           SET ELEMENT-COLUMN-INDEX TO 1.
           MOVE ZERO TO PRINT-LINE-FLAG.
           PERFORM 8811-FILL-DIST-PRINT-LINE
                   UNTIL PRINT-LINE-FILLED.
           WRITE PRINT-RECORD-2
                   FROM ELEMENT-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 1 LINE.
           MOVE SPACES TO ELEMENT-TOTAL-DETAIL-LINE.
           SET ORIGINAL-INDEX-VALUE
                   TO DISTRICT-ELEMENT-ROW-INDEX.
           SEARCH DISTRICT-ELEMENT-TOTALS-ROWS
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO DISTRICT-ELEMENT-ROW-INDEX
               NEXT SENTENCE.
           SET EDIT-ERROR-LITERAL-INDEX
                   TO DISTRICT-ELEMENT-ROW-INDEX.
      *
      *
       8811-FILL-DIST-PRINT-LINE.
           SET  ELEMENT-TABLE-ROW-ID
                   TO DISTRICT-ELEMENT-ROW-INDEX.
           SET  ELEMENT-TABLE-COLUMN-ID
                   TO DISTRICT-ELEMENT-COLUMN-INDEX.
           IF (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-2)
                       OR
                  (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-3)
                       OR
                  (ELEMENT-ROWS-1-25
                   AND ELEMENT-COLUMN-4)
      *                OR
      *           (ELEMENT-ROWS-13
      *            AND ELEMENT-COLUMN-4)
      *                OR
      *           (ELEMENT-ROWS-17
      *            AND ELEMENT-COLUMN-4)
               MOVE NOT-APPLICABLE-LITERAL
                       TO ELEMENT-N-A
                       IN ELEMENT-COLUMNS
                       (ELEMENT-COLUMN-INDEX)
           ELSE
               MOVE DISTRICT-ELEMENT-TOTAL
                       IN DISTRICT-ELEMNT-TOTALS-COLUMNS
                       (DISTRICT-ELEMENT-ROW-INDEX
                        DISTRICT-ELEMENT-COLUMN-INDEX)
                       TO ELEMENT-TOTAL
                       IN ELEMENT-COLUMNS
                       (ELEMENT-COLUMN-INDEX).
           SET ORIGINAL-INDEX-VALUE
                   TO DISTRICT-ELEMENT-COLUMN-INDEX.
           SEARCH DISTRICT-ELEMNT-TOTALS-COLUMNS
           AT END
               MOVE 1 TO PRINT-LINE-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                   IS NOT EQUAL TO DISTRICT-ELEMENT-COLUMN-INDEX
               NEXT SENTENCE.
           SET ELEMENT-COLUMN-INDEX
                   TO DISTRICT-ELEMENT-COLUMN-INDEX.
      *
      *
       8820-PRINT-DIST-INTGRTY-TOTALS.
           MOVE 5 TO HEADER-CONTROL-FLAG.
           MOVE SUM2-PROGRAM-LIT TO REPORT-ID-LIT.
           MOVE SUMMARY-PROGRAM-NAME
                   IN PROGRAM-LITERAL-AND-WORK
                   TO PROGRAM-NAME
                   IN HEADER-REPORT-ID.
           PERFORM 85002-PRINT-HEADERS.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-1-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-1-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-2-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-2-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-3-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-3-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-4-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-4-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-5-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-5-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-6-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-6-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-7-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-7-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-8-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-8-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-9-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-9-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-10-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-10-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-11-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-11-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-12-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-12-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-13-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-13-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-14-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-14-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-15-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-15-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-RECORD-2.
           MOVE DIST-INTEGRITY-ERR-16-TOTAL TO INTEGRITY-TOTAL.
           MOVE INTEGRITY-ERR-16-DESC TO INTEGRITY-DESCRIPTION.
           WRITE PRINT-RECORD-2
                   FROM INTEGRITY-TOTAL-DETAIL-LINE
                   AFTER ADVANCING 2 LINES.
      *    MOVE SPACES TO PRINT-RECORD-2.
      *    MOVE DIST-INTEGRITY-ERR-17-TOTAL TO INTEGRITY-TOTAL.
      *    MOVE INTEGRITY-ERR-17-DESC TO INTEGRITY-DESCRIPTION.
      *    WRITE PRINT-RECORD-2
      *            FROM INTEGRITY-TOTAL-DETAIL-LINE
      *            AFTER ADVANCING 2 LINES.
      *
      *
       8830-PRINT-DISTRICT-SUM-TOTALS.
           MOVE TOTAL-DESCRIPTION
                   IN TOTAL-DESCRIPTION-ENTRY
                   (TOTAL-DESCRIPTION-INDEX)
                   TO TOTAL-DESCRIPTION
                   IN TOTAL-DETAIL-LINE.
           MOVE DISTRICT-TOTAL
                   IN TOTAL-DISTRICT-ENTRY
                   (TOTAL-DISTRICT-INDEX)
                   TO TOTAL-FIELD
                   IN TOTAL-DETAIL-LINE.
           WRITE PRINT-RECORD-2
                   FROM TOTAL-DETAIL-LINE
                   AFTER ADVANCING 1 LINE.
           SET ORIGINAL-INDEX-VALUE
                   TO TOTAL-DISTRICT-INDEX.
           SEARCH TOTAL-DISTRICT-ENTRY
           AT END
               MOVE 1 TO TABLE-SEARCH-FLAG
           WHEN ORIGINAL-INDEX-VALUE
                    IS NOT EQUAL TO TOTAL-DISTRICT-INDEX
           SET TOTAL-DESCRIPTION-INDEX
                   TO TOTAL-DISTRICT-INDEX.
      *
      *
       9000-PROGRAM-FINALIZATION.
           PERFORM 2200-PROCESS-CNTL-BREAK.
           PERFORM 8800-PRINT-DISTRICT-TOTALS.
           CLOSE COURSE-INV-DATA-FILE.
           CLOSE EDITED-COURSE-INV-DATA-FILE.
           CLOSE REPORT-TOTALS-FILE.
           CLOSE DATAEDIT-ERROR-REPORT.
           CLOSE SUMMARY-ERROR-REPORT.
           CLOSE TOPCODE-CHECK-FILE.
       END PROGRAM CBEDIT.