       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGRAM4.
       AUTHOR.        REESE BAKER.

      *****************************************************************
      * This program validates warehouse, vendor, and product code for
      * the incoming files. When the valid codes are found, the report
      * is written. If the incoming files aren't found, they are put
      * into an error file. Files are sorted and merged.
      *
      * INPUT: KEY FIELDS
      * -WAREHOUSE ID, VENDOR ID, PRODUCT ID 
      * 
      * OUTPUT: The inventory file produced has only the validated
      * information for the key fields. Totals for the costs of these
      * fields are reported at the bottom, as well as an error count.
      * Product names, sizes, and types are validated and changed
      * accordingly. 
      *
      * CALCULATIONS:
      * Stock X price = total cost
      * Accumulate product total cost
      * Accumulate vendor total cost
      * Accumulate warehouse total cost
      * Accumulate grand total cost
      *
      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  PC.
       OBJECT-COMPUTER.  PC.



       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

         SELECT VIBES-FILE
             ASSIGN TO 'PR4F22-VibesInvenmoreerrors.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT SORTED-VIBES-FILE
             ASSIGN TO 'SortedVibesInven.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT LAX1-FILE
             ASSIGN TO 'PR4F22-LAX1.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT SEA1-FILE
             ASSIGN TO 'PR4F22-SEA1.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT SLC1-FILE
             ASSIGN TO 'PR4F-SLC1.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT SLC2-FILE
             ASSIGN TO 'PR4F22-SLC2.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT ERROR-FILE
             ASSIGN TO 'ERROR.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT SORT-MERGE-FILE
             ASSIGN TO 'SORT.TMP'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT UTAH-FILE
             ASSIGN TO 'UTAH-FILE.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

         SELECT INVENTORY-REPORT
             ASSIGN TO 'INENTORY-REPORT.TXT'
             ORGANIZATION IS LINE SEQUENTIAL.

      *

       DATA DIVISION.
       FILE SECTION.

       FD VIBES-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 VIBES-REC.
         05 VR-WAREHOUSE-ID              PIC X(4).
         05 VR-VENDOR-ID                 PIC A(1).
         05 VR-PRODUCT-ID                PIC X(3).
         05 VR-PRODUCT-ARRAY       OCCURS 5 TIMES.
             10 VR-PRODUCT-NAME          PIC X(13).
             10 VR-PRODUCT-SIZE          PIC A(1).
             10 VR-PRODUCT-TYPE          PIC A(1).
             10 VR-NUM-IN-STOCK          PIC 9(4).
             10 VR-PURCHASE-PRICE        PIC S999V99.

      *

       FD SORTED-VIBES-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 SORTED-VIBES-REC.
         05 SVR-WAREHOUSE-ID             PIC X(4).
         05 SVR-VENDOR-ID                PIC A(1).
         05 SVR-PRODUCT-ID               PIC X(3).
         05 SVR-PRODUCT-ARRAY      OCCURS 5 TIMES.
             10 SVR-PRODUCT-NAME         PIC X(13).
             10 SVR-PRODUCT-SIZE         PIC A(1).
             10 SVR-PRODUCT-TYPE         PIC A(1).
             10 SVR-NUM-IN-STOCK         PIC 9(4).
             10 SVR-PURCHASE-PRICE       PIC S999V99.
 
      *

       FD LAX1-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 LAX1-REC.
         05 LAX-WAREHOUSE-ID              PIC X(4).
         05 LAX-VENDOR-ID                 PIC A(1).
         05 LAX-PRODUCT-ID                PIC X(3).
         05 LAX-PRODUCT-ARRAY       OCCURS 5 TIMES.
             10 LAX-PRODUCT-NAME          PIC X(13).
             10 LAX-PRODUCT-SIZE          PIC A(1).
             10 LAX-PRODUCT-TYPE          PIC A(1).
             10 LAX-NUM-IN-STOCK          PIC 9(4).
             10 LAX-PURCHASE-PRICE        PIC S999V99.

      *

       FD SEA1-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 SEA1-REC.
         05 SEA-WAREHOUSE-ID             PIC X(4).
         05 SEA-VENDOR-ID                PIC A(1).
         05 SEA-PRODUCT-ID               PIC X(3).
         05 SEA-PRODUCT-ARRAY      OCCURS 5 TIMES.
             10 SEA-PRODUCT-NAME         PIC X(13).
             10 SEA-PRODUCT-SIZE         PIC A(1).
             10 SEA-PRODUCT-TYPE         PIC A(1).
             10 SEA-NUM-IN-STOCK         PIC 9(4).
             10 SEA-PURCHASE-PRICE       PIC S999V99.

      *

       FD SLC1-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 SLC1-REC.
         05 SLC1-WAREHOUSE-ID            PIC X(4).
         05 SLC1-VENDOR-ID               PIC A(1).
         05 SLC1-PRODUCT-ID              PIC X(3).
         05 SLC1-PRODUCT-ARRAY     OCCURS 5 TIMES.
             10 SLC1-PRODUCT-NAME        PIC X(13).
             10 SLC1-PRODUCT-SIZE        PIC A(1).
             10 SLC1-PRODUCT-TYPE        PIC A(1).
             10 SLC1-NUM-IN-STOCK        PIC 9(4).
             10 SLC1-PURCHASE-PRICE      PIC S999V99.

      *

       FD SLC2-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 SLC2-REC.
         05 SLC2-WAREHOUSE-ID            PIC X(4).
         05 SLC2-VENDOR-ID               PIC A(1).
         05 SLC2-PRODUCT-ID              PIC X(3).
         05 SLC2-PRODUCT-ARRAY     OCCURS 5 TIMES.
             10 SLC2-PRODUCT-NAME        PIC X(13).
             10 SLC2-PRODUCT-SIZE        PIC A(1).
             10 SLC2-PRODUCT-TYPE        PIC A(1).
             10 SLC2-NUM-IN-STOCK        PIC 9(4).
             10 SLC2-PURCHASE-PRICE      PIC S999V99.

      *

       FD ERROR-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 ERROR-REC.
         05 ER-WAREHOUSE-ID              PIC X(4).
         05 ER-VENDOR-ID                 PIC A(1).
         05 ER-PRODUCT-ID                PIC X(3).
         05 ER-PRODUCT-ARRAY       OCCURS 5 TIMES.
             10 ER-PRODUCT-NAME          PIC X(13).
             10 ER-PRODUCT-SIZE          PIC A(1).
             10 ER-PRODUCT-TYPE          PIC A(1).
             10 ER-NUM-IN-STOCK          PIC 9(4).
             10 ER-PURCHASE-PRICE        PIC S999V99.

      *

       FD UTAH-FILE
         RECORD CONTAINS 128 CHARACTERS. 

       01 UTAH-REC.
         05 UR-WAREHOUSE-ID              PIC X(4).
         05 UR-VENDOR-ID                 PIC A(1).
         05 UR-PRODUCT-ID                PIC X(3).
         05 UR-PRODUCT-ARRAY       OCCURS 5 TIMES.
             10 UR-PRODUCT-NAME          PIC X(13).
             10 UR-PRODUCT-SIZE          PIC A(1).
             10 UR-PRODUCT-TYPE          PIC A(1).
             10 UR-NUM-IN-STOCK          PIC 9(4).
             10 UR-PURCHASE-PRICE        PIC S999V99.

      *

       SD SORT-MERGE-FILE
         RECORD CONTAINS 128 CHARACTERS.

       01 SORT-MERGE-REC.
         05 SR-WAREHOUSE-ID              PIC X(4).
         05 SR-VENDOR-ID                 PIC A(1).
         05 SR-PRODUCT-ID                PIC X(3).
         05 FILLER                       PIC X(120).

      *

       FD INVENTORY-REPORT
         RECORD CONTAINS 81 CHARACTERS.

      *

       01 REPORT-LINE                PIC X(81).

      *

       WORKING-STORAGE SECTION.

       01 FLAGS-N-SWITCHES.
         05 EOF-FLAG                     PIC X VALUE SPACE.
             88 MORE-RECORDS                   VALUE 'Y'.
             88 NO-MORE-RECORDS                VALUE 'N'.
         05 FIRST-RECORD                 PIC X VALUE 'Y'.
         05 SUB                          PIC 9.

      *

       01 REPORT-FIELDS.
         05 PROPER-SPACING               PIC  9 VALUE 1.
         05 ERROR-COUNT                  PIC S9 VALUE 0.

      *

       01 WS-CURRENT-DATE.
         05 WS-MONTH                     PIC 99.
         05 WS-DAY                       PIC 99.
         05 WS-YEAR                      PIC 9999.

      *

       01 HOLD-FIELD.
         05 HF-WAREHOUSE-ID              PIC X(4).
         05 HF-VENDOR-ID                 PIC A(1).
         05 HF-PRODUCT-ID                PIC X(3).
         05 HF-PRICE                     PIC S9(7)V99.

      *

       01 ERROR-FIELD.
         05 FILLER                       PIC X(13) VALUE SPACE.
         05                              PIC X(14) VALUE
                                             'TOTAL ERRORS: '.
         05 EF-ERROR-COUNT               PIC 99.

      *

       01 TOTAL-FIELD.
         05 TF-PRODUCT-COST              PIC S9(8)V99.
         05 TF-VENDOR-COST               PIC S9(9)V99.
         05 TF-WAREHOUSE-COST            PIC S9(10)V99.
         05 TF-GRAND-TOTAL-COST          PIC S9(11)V99.

      *

      ***********************TABLE*************************

       01 VENDOR-NAME.
         05 PIC X(16)        VALUE 'MMad Hatter Oils'.
         05 PIC X(16)        VALUE 'PPure Creams'.
         05 PIC X(16)        VALUE 'CCheebs Herbs'.

      *

       01 VENDOR-TABLE REDEFINES
             VENDOR-NAME OCCURS 3 TIMES INDEXED
                 BY VENDOR-INDEX.
         05 VEND-ID                      PIC A(1).
         05 VEND-NAME                    PIC X(15).

      *****************************************************


      *

       01 HEADING-ONE.
         05 FILLER                   PIC X(34) VALUE SPACE.
         05                          PIC X(13) VALUE
                                         'HEALING VIBES'.
         05 FILLER                   PIC X(34) VALUE SPACE.

      *

       01 HEADING-TWO.
         05 FILLER                   PIC X(10) VALUE SPACE.
         05 H2-DATE.
             10 H1-MONTH             PIC 99.
             10                      PIC X VALUE '/'.
             10 H1-DAY               PIC 99.
             10                      PIC X VALUE '/'.
             10 H1-YEAR              PIC 9999.
         05 FILLER                   PIC X(13) VALUE SPACE.
         05                          PIC X(16) VALUE
                                         'INVENTORY REPORT'.
         05 FILLER                   PIC X(11) VALUE SPACE.
         05                          PIC X(4) VALUE 'UTAH'.
         05 FILLER                   PIC X(17) VALUE SPACE.

      *

       01 WAREHOUSE-HEADER.
         05 FILLER                   PIC X(2) VALUE SPACE.
         05                          PIC X(11) VALUE
                                         'WAREHOUSE: '.
         05 WAREHOUSE-ID             PIC X(16).
         05 FILLER                   PIC X(52) VALUE SPACE.

      *

       01 VENDOR-HEADER.
         05 FILLER                   PIC X(5) VALUE SPACE.
         05                          PIC X(8) VALUE SPACE.
         05 VENDOR-ID                PIC X(15).
         05 FILLER                   PIC X(53) VALUE SPACE.

      *

       01 HEADING-THREE.
         05 FILLER                   PIC X(8) VALUE SPACE.
         05                          PIC X(7) VALUE
                                         'PRODUCT'.
         05 FILLER                   PIC X(7) VALUE SPACE.
         05                          PIC X(4) VALUE 'PROD'.
         05 FILLER                   PIC X(4) VALUE SPACE.
         05                          PIC X(7) VALUE
                                         'PRODUCT'.
         05 FILLER                   PIC X(5) VALUE SPACE.
         05                          PIC X(4) VALUE 'PROD'.
         05 FILLER                   PIC X(5) VALUE SPACE.
         05                          PIC X(2) VALUE 'IN'.
         05 FILLER                   PIC X(7) VALUE SPACE.
         05                          PIC X(5) VALUE 'TOTAL'.
         05 FILLER                   PIC X(16) VALUE SPACE.

      *

       01 HEADING-FOUR.
         05 FILLER                   PIC X(10) VALUE SPACE.
         05                          PIC X(4) VALUE 'NAME'.
         05 FILLER                   PIC X(9) VALUE SPACE.
         05                          PIC X(2) VALUE 'ID'.
         05 FILLER                   PIC X(6) VALUE SPACE.
         05                          PIC X(4) VALUE 'SIZE'.
         05 FILLER                   PIC X(7) VALUE SPACE.
         05                          PIC X(4) VALUE 'TYPE'.
         05 FILLER                   PIC X(4) VALUE SPACE.
         05                          PIC X(5) VALUE 'STOCK'.
         05 FILLER                   PIC X(5) VALUE SPACE.
         05                          PIC X(4) VALUE 'COST'.
         05 FILLER                   PIC X(17) VALUE SPACE.

      *

       01 DETAIL-LINE.
         05 FILLER                   PIC X(5) VALUE SPACE.
         05 DL-PRODUCT-NAME          PIC X(13).
         05 FILLER                   PIC X(4) VALUE SPACE.
         05 DL-PRODUCT-ID            PIC X(3).
         05 FILLER                   PIC X(3) VALUE SPACE. 
         05 DL-PRODUCT-SIZE          PIC X(11).
         05 FILLER                   PIC X(3) VALUE SPACE.
         05 DL-PRODUCT-TYPE          PIC X(5).
         05 FILLER                   PIC X(3) VALUE SPACE.
         05 DL-IN-STOCK              PIC Z999.
         05 FILLER                   PIC X(3) VALUE SPACE.
         05 DL-TOTAL-COST            PIC $,ZZZ,ZZZ.99.
         05 FILLER                   PIC X(12) VALUE SPACE.

      *

       01 PRODUCT-TOTAL-LINE.
         05 FILLER                   PIC X(15) VALUE SPACE.
         05                          PIC X(15) VALUE
                                         'TOTAL PRODUCT: '.
         05 PRODUCT-TOTAL-NAME       PIC X(13).
         05 FILLER                   PIC X(13) VALUE SPACE.
         05 PRODUCT-TOTAL-COST       PIC $Z,ZZZ,ZZZ.99.
         05 FILLER                   PIC X(12) VALUE SPACE.

      *

       01 VENDOR-TOTAL-LINE.
         05 FILLER                   PIC X(12) VALUE SPACE.
         05                          PIC X(18) VALUE
                                      'TOTAL FOR VENDOR: '.
         05 VENDOR-TOTAL-NAME        PIC X(15).
         05 FILLER                   PIC X(10) VALUE SPACE.
         05 VENDOR-TOTAL-COST        PIC $Z,ZZZ,ZZZ.99.
         05 FILLER                   PIC X(12) VALUE SPACE. 

      *

       01 WAREHOUSE-TOTAL-LINE.
         05 FILLER                   PIC X(9) VALUE SPACE.
         05                          PIC X(21) VALUE
                                     'TOTAL FOR WAREHOUSE: '.
         05 WAREHOUSE-TOTAL-NAME     PIC X(16).
         05 FILLER                   PIC X(7) VALUE SPACE.
         05 WAREHOUSE-TOTAL-COST     PIC $Z,ZZZ,ZZZ.99.
         05 FILLER                   PIC X(12) VALUE SPACE.

      *

       01 GRAND-TOTAL-LINE.
         05 FILLER                   PIC X(17) VALUE SPACE.
         05                          PIC X(13) VALUE
                                         'GRAND TOTAL: '.
         05                          PIC X(4) VALUE 'UTAH'.
         05 FILLER                   PIC X(18) VALUE SPACE.
         05 GRAND-TOTAL-COST         PIC $Z,ZZZ,ZZZ,ZZZ.99.

      *

       01 ERROR-TOTAL-LINE.
         05 FILLER                   PIC X(17) VALUE SPACE.
         05                          PIC X(13) VALUE
                                         'ERROR TOTAL: '.
         05 FILLER                   PIC X(34) VALUE SPACE.
         05 ERROR-TOTAL              PIC ZZ9.
      
      *

       PROCEDURE DIVISION.

       100-PRINT-REPORT.
         PERFORM 110-MAIN-MODULE
         PERFORM 115-HOUSEKEEPING
         PERFORM 125-READ-FILE
         PERFORM 500-FINAL-ROUTINE

       .

       110-MAIN-MODULE.

         SORT SORT-MERGE-FILE
             ON ASCENDING KEY SR-WAREHOUSE-ID,
                              SR-VENDOR-ID,
                              SR-PRODUCT-ID
             USING VIBES-FILE
             GIVING SORTED-VIBES-FILE

      *

         OPEN INPUT  SORTED-VIBES-FILE
              OUTPUT LAX1-FILE,
                     SEA1-FILE,
                     SLC1-FILE, 
                     SLC2-FILE,
                     ERROR-FILE
 
      *

         PERFORM UNTIL NO-MORE-RECORDS
             READ SORTED-VIBES-FILE
                 AT END
                     MOVE 'N' TO EOF-FLAG
                 NOT AT END
                     EVALUATE TRUE
                         WHEN SVR-WAREHOUSE-ID = 'LAX1'
                             MOVE SORTED-VIBES-REC TO LAX1-REC
                                 WRITE LAX1-REC
                         WHEN SVR-WAREHOUSE-ID = 'SEA1'
                             MOVE SORTED-VIBES-REC TO SEA1-REC
                                 WRITE SEA1-REC
                         WHEN SVR-WAREHOUSE-ID = 'SLC1'
                             MOVE SORTED-VIBES-REC TO SLC1-REC
                                 WRITE SLC1-REC
                         WHEN SVR-WAREHOUSE-ID = 'SLC2'
                             MOVE SORTED-VIBES-REC TO SLC2-REC
                                 WRITE SLC2-REC
                         WHEN OTHER
                             MOVE SORTED-VIBES-REC TO ERROR-REC
                                 WRITE ERROR-REC
                             ADD 1 TO ERROR-COUNT
                     END-EVALUATE
             END-READ
         END-PERFORM

      *

         CLOSE LAX1-FILE,
               SEA1-FILE,
               SLC1-FILE,
               SLC2-FILE,
               ERROR-FILE,
               SORTED-VIBES-FILE

      *

         MOVE SPACE TO EOF-FLAG

         MERGE SORT-MERGE-FILE
             ON ASCENDING KEY SR-WAREHOUSE-ID,
                              SR-VENDOR-ID,
                              SR-PRODUCT-ID
             USING SLC1-FILE,
                   SLC2-FILE
             GIVING UTAH-FILE

       .

      *


       115-HOUSEKEEPING.

         OPEN INPUT  UTAH-FILE
              OUTPUT INVENTORY-REPORT

         ACCEPT WS-CURRENT-DATE FROM DATE

         MOVE WS-MONTH TO H1-MONTH
         MOVE WS-DAY   TO H1-DAY
         MOVE WS-YEAR  TO H1-YEAR

         PERFORM 120-HEADING-ROUTINE-ONE

       .

       120-HEADING-ROUTINE-ONE.

         WRITE REPORT-LINE FROM HEADING-ONE
             AFTER ADVANCING PROPER-SPACING

         MOVE HEADING-TWO TO REPORT-LINE
         WRITE REPORT-LINE FROM HEADING-TWO
             AFTER ADVANCING PROPER-SPACING

       .

       125-READ-FILE.

         PERFORM UNTIL NO-MORE-RECORDS
             READ UTAH-FILE
                 AT END
                     MOVE 'N' TO EOF-FLAG
                 NOT AT END
                  IF UR-WAREHOUSE-ID = 'SLC1' OR 'SLC2'
                     PERFORM 130-PROCESS-RECORD
                  END-IF
             END-READ
         END-PERFORM

       .


       130-PROCESS-RECORD.

         EVALUATE TRUE
             WHEN FIRST-RECORD = 'Y'
                 MOVE 'N' TO FIRST-RECORD

                 MOVE UR-WAREHOUSE-ID TO HF-WAREHOUSE-ID
                 MOVE UR-VENDOR-ID    TO HF-VENDOR-ID
                 MOVE UR-PRODUCT-ID   TO HF-PRODUCT-ID

                 PERFORM 140-WAREHOUSE-HEADING-ROUTINE
                 PERFORM 155-VENDOR-HEADING-ROUTINE
                 PERFORM 145-HEADING-ROUTINE-TWO

             WHEN UR-WAREHOUSE-ID NOT = HF-WAREHOUSE-ID
                 PERFORM 200-WAREHOUSE-BREAK
                 PERFORM 140-WAREHOUSE-HEADING-ROUTINE
                 PERFORM 155-VENDOR-HEADING-ROUTINE
                 PERFORM 145-HEADING-ROUTINE-TWO

             WHEN HF-VENDOR-ID NOT = UR-VENDOR-ID
                 PERFORM 300-VENDOR-BREAK
                 PERFORM 155-VENDOR-HEADING-ROUTINE
                 PERFORM 145-HEADING-ROUTINE-TWO

             WHEN UR-PRODUCT-ID NOT = HF-PRODUCT-ID
                 PERFORM 400-PRODUCT-BREAK
                 PERFORM 145-HEADING-ROUTINE-TWO
         END-EVALUATE

         PERFORM 150-DATA-VALIDATION     

       .

       135-SEARCH-VENDOR.
         
         SET VENDOR-INDEX TO 1

         SEARCH VENDOR-TABLE
             AT END
                MOVE 'INVALID' TO  VENDOR-ID,
                                   VENDOR-TOTAL-NAME
             WHEN UR-VENDOR-ID = VEND-ID (VENDOR-INDEX)
                 MOVE VENDOR-NAME TO VENDOR-ID,
                                     VENDOR-TOTAL-NAME
         END-SEARCH


       .

       140-WAREHOUSE-HEADING-ROUTINE.

      
         MOVE UR-WAREHOUSE-ID TO WAREHOUSE-ID

         EVALUATE TRUE
             WHEN WAREHOUSE-ID = 'SLC1'
                 MOVE 'UTAH WAREHOUSE 1' TO WAREHOUSE-ID,
                                       WAREHOUSE-TOTAL-NAME
                 MOVE WAREHOUSE-HEADER TO REPORT-LINE
                 WRITE REPORT-LINE FROM WAREHOUSE-HEADER
                     AFTER ADVANCING 2 LINES
             WHEN WAREHOUSE-ID = 'SLC2'
                 MOVE 'UTAH WAREHOUSE 2' TO WAREHOUSE-ID,
                                       WAREHOUSE-TOTAL-NAME
                 MOVE WAREHOUSE-HEADER TO REPORT-LINE
                 WRITE REPORT-LINE FROM WAREHOUSE-HEADER
                     AFTER ADVANCING 2 LINES
         END-EVALUATE

       .

       145-HEADING-ROUTINE-TWO.

         WRITE REPORT-LINE FROM HEADING-THREE
             AFTER ADVANCING 2 LINES

         MOVE 1 TO PROPER-SPACING

         WRITE REPORT-LINE FROM HEADING-FOUR
             AFTER ADVANCING PROPER-SPACING

       .

       150-DATA-VALIDATION.

         PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             EVALUATE TRUE
                 WHEN SUB = 1
                     MOVE UR-PRODUCT-NAME(SUB) TO DL-PRODUCT-NAME,
                                                  PRODUCT-TOTAL-NAME
                 WHEN OTHER
                     MOVE SPACES TO DL-PRODUCT-NAME, 
             END-EVALUATE

         MOVE UR-PRODUCT-ID TO DL-PRODUCT-ID

         EVALUATE UR-PRODUCT-SIZE(SUB)
             WHEN 'X'
                 MOVE 'Extra Large' TO DL-PRODUCT-SIZE
             WHEN 'L'
                 MOVE 'Large' TO DL-PRODUCT-SIZE
             WHEN 'M'
                 MOVE 'Medium' TO DL-PRODUCT-SIZE
             WHEN 'S'
                 MOVE 'Small' TO DL-PRODUCT-SIZE
             WHEN 'A'
                 MOVE 'Sample' TO DL-PRODUCT-SIZE
             WHEN OTHER
                 MOVE 'BAD' TO DL-PRODUCT-SIZE
         END-EVALUATE

         EVALUATE UR-PRODUCT-TYPE(SUB)
             WHEN 'C'
                 MOVE 'Cream' TO DL-PRODUCT-TYPE
             WHEN 'O'
                 MOVE 'Oil' TO DL-PRODUCT-TYPE
             WHEN OTHER 
                 MOVE 'BAD' TO DL-PRODUCT-TYPE
         END-EVALUATE

         EVALUATE UR-NUM-IN-STOCK(SUB)
             WHEN NOT NUMERIC
                 MOVE ZEROS TO DL-IN-STOCK, UR-NUM-IN-STOCK(SUB)
             WHEN NUMERIC
                 MOVE UR-NUM-IN-STOCK(SUB) TO DL-IN-STOCK
         END-EVALUATE

         EVALUATE UR-PURCHASE-PRICE(SUB)
             WHEN NOT NUMERIC
                 MOVE ZEROS TO DL-TOTAL-COST, UR-PURCHASE-PRICE(SUB)
             WHEN NUMERIC
                 MOVE UR-PURCHASE-PRICE(SUB) TO DL-TOTAL-COST
         END-EVALUATE

         COMPUTE HF-PRICE = UR-NUM-IN-STOCK(SUB) *
                             UR-PURCHASE-PRICE(SUB)

         COMPUTE TF-PRODUCT-COST = HF-PRICE +
                             TF-PRODUCT-COST

         COMPUTE TF-VENDOR-COST = HF-PRICE +
                             TF-VENDOR-COST

         COMPUTE TF-WAREHOUSE-COST = HF-PRICE +
                             TF-WAREHOUSE-COST

         COMPUTE TF-GRAND-TOTAL-COST = HF-PRICE +
                             TF-GRAND-TOTAL-COST

         MOVE HF-PRICE TO DL-TOTAL-COST

         WRITE REPORT-LINE FROM DETAIL-LINE
             AFTER ADVANCING PROPER-SPACING
         

        END-PERFORM

       . 

       155-VENDOR-HEADING-ROUTINE.

         PERFORM 135-SEARCH-VENDOR

         WRITE REPORT-LINE FROM VENDOR-HEADER
             AFTER ADVANCING 2 LINES

       . 


       160-GRAND-TOTAL.
         MOVE TF-GRAND-TOTAL-COST TO GRAND-TOTAL-COST

         MOVE GRAND-TOTAL-LINE TO REPORT-LINE
         WRITE REPORT-LINE FROM GRAND-TOTAL-LINE
             AFTER ADVANCING 3 LINES
       .

       200-WAREHOUSE-BREAK.
        
         PERFORM 300-VENDOR-BREAK

         MOVE HF-WAREHOUSE-ID TO WAREHOUSE-ID
         MOVE TF-WAREHOUSE-COST TO WAREHOUSE-TOTAL-COST

         MOVE WAREHOUSE-TOTAL-LINE TO REPORT-LINE
         WRITE REPORT-LINE FROM WAREHOUSE-TOTAL-LINE
             AFTER ADVANCING 2 LINES

         MOVE ZEROS TO WAREHOUSE-TOTAL-COST
         MOVE ZEROS TO TF-WAREHOUSE-COST

         MOVE UR-WAREHOUSE-ID TO HF-WAREHOUSE-ID

       .

       300-VENDOR-BREAK.
         
         PERFORM 400-PRODUCT-BREAK

         MOVE TF-VENDOR-COST TO VENDOR-TOTAL-COST

         MOVE VENDOR-TOTAL-LINE TO REPORT-LINE
         WRITE REPORT-LINE FROM VENDOR-TOTAL-LINE
             AFTER ADVANCING 2 LINES

         MOVE ZEROS TO VENDOR-TOTAL-COST
         MOVE ZEROS TO TF-VENDOR-COST

         MOVE UR-VENDOR-ID TO HF-VENDOR-ID

       .

       400-PRODUCT-BREAK.

         MOVE TF-PRODUCT-COST TO PRODUCT-TOTAL-COST

         MOVE PRODUCT-TOTAL-LINE TO REPORT-LINE
         WRITE REPORT-LINE FROM PRODUCT-TOTAL-LINE
             AFTER ADVANCING 2 LINES

         MOVE ZEROS TO PRODUCT-TOTAL-COST
         MOVE ZEROS TO TF-PRODUCT-COST

         MOVE UR-PRODUCT-ID TO HF-PRODUCT-ID

       .

       450-END-OF-JOB.
         PERFORM 200-WAREHOUSE-BREAK

         MOVE ERROR-COUNT TO EF-ERROR-COUNT
         WRITE REPORT-LINE FROM ERROR-FIELD
             AFTER ADVANCING 2 LINES

       .

       500-FINAL-ROUTINE.

         PERFORM 450-END-OF-JOB
         PERFORM 160-GRAND-TOTAL



         CLOSE UTAH-FILE,
               INVENTORY-REPORT

         STOP RUN

       .

         
