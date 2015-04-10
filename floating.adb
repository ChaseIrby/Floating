-- Name: Chase Irby
-- Date: February 27, 2014
-- Course: ITEC 320 Principles of Procedural Programming

-- Purpose: This program reads standard input and proceeds
--   to transform the input into 7 different forms.

-- Input is a sequence of integers and floats (pos or neg),
--   stored as F, until end of file.

-- Output is the operation, a space, and the result.

-- Input is assumed to be valid but exceptions are handled.

-- Sample input:

-- 23.32399940490722660000000
-- 0
-- -.25

-- Corresponding output:

-- As entered:                     23.32399940490722660000000
-- Decimal scientific notation:    2.3323999405E+01
-- Binary scientific notation:     1.0111 0101 0010 1111 0001 101 * 2**4
-- Calculated FP representation: 0 1000 0011 0111 0101 0010 1111 0001 101
-- Conversion to bit array:      0 1000 0011 0111 0101 0010 1111 0001 101
-- Put to string in FP rep.:    2# 1000 0011 0111 0101 0010 1111 0001 101#
-- Put in FP representation:

-- As entered:                     0.00000000000000000000000
-- Decimal scientific notation:    0.0000000000E+00
-- Binary scientific notation:     0.0000 0000 0000 0000 0000 000 * 2**0
-- Calculated FP representation: 0 0000 0000 0000 0000 0000 0000 0000 000
-- Conversion to bit array:      0 0000 0000 0000 0000 0000 0000 0000 000
-- Put to string in FP rep.:                                          2#0#
-- Put IN FP Representation:           2#0#

-- As entered:                     -0.25000000000000000000000
-- Decimal scientific notation:    -2.5000000000E-01
-- Binary scientific notation:     -1.0000 0000 0000 0000 0000 000 * 2**-2
-- Calculated FP representation: 1 0111 1101 0000 0000 0000 0000 0000 000
-- Conversion to bit array:      1 0111 1101 0000 0000 0000 0000 0000 000
-- Put to string in FP rep.:   2#1 0111 1101 0000 0000 0000 0000 0000 000#
-- Put in FP representation:    2#10111110100000000000000000000000#

WITH Ada.Integer_Text_Io; USE Ada.Integer_Text_Io;
WITH Ada.Text_Io; USE Ada.Text_Io;
WITH Ada.Float_Text_Io; USE Ada.Float_Text_Io;
WITH Unchecked_Conversion;

PROCEDURE Floating IS

   ------------------------------------------------------------------
   -- Purpose: Handles conversion to bit array (step 5)
   -- Parameters: Passed_Float (Float to be made into bit array,
   --   Index_Int (Index to be printed out)
   -- Local vars: BitArray (BitString) holds the float's conversion
   --                to bits
   -- Precondition: Parameters are valid
   -- Postcondition: Prints out value of index
   ------------------------------------------------------------------

   PROCEDURE Fill_Array (Passed_Float : IN Float;
         Index_Int : IN Integer) IS

      TYPE BitString IS ARRAY (1 .. 32) OF Integer RANGE 0 .. 1;
      FOR  BitString'Component_Size USE 1;
      FOR BitString'Size USE 32;

      FUNCTION Copy_Bits IS
      NEW Unchecked_Conversion(
         Source => Float,
         Target => BitString);

      BitArray : BitString := Copy_Bits (Passed_Float);

      BEGIN

         Put(BitArray(Index_Int), Width => 0);


      END Fill_Array;

   ------------------------------------------------------------------
   -- Purpose: Converts float to unsigned32 and string representation
   -- Parameters: Passed_Float (converted to Unsigned32)
   -- Local vars: Some_Int (unsigned32) holds the conversion
   --             S (String)
   -- Precondition: Parameter is float
   -- Postcondition: Prints Some_Int as String and Floating Point
   ------------------------------------------------------------------

   PROCEDURE Unsigned_Float (Passed_Float: Float) IS
      TYPE Unsigned32 IS mod 2**32;
      PACKAGE Unsigned32_Io IS NEW Ada.Text_Io.Modular_Io(Unsigned32);
      USE Unsigned32_Io;
      FUNCTION Convert IS NEW Unchecked_Conversion(Source => Float,
         Target => Unsigned32);

      Some_Int: Unsigned32 := Convert(Passed_Float);
      S: String(1 .. 35);

   BEGIN

      Put("Put to string in FP rep.:       ");
      Unsigned32_Io.Put(To => S, Item => Some_Int, Base => 2);

      Put(S(1 .. 3));

      FOR I IN 4 .. 35 LOOP

         IF I MOD 4 = 0 THEN
            Put(" ");
         END IF;

         Put(S(I));

      END LOOP;

      New_Line;

      Put("Put in FP representation:       ");
      Put(Some_Int, Base => 2, Width => 35);

      New_Line;
      Put("-----------------------------------------");

   END Unsigned_Float;

   ------------------------------------------------------------------
   -- Purpose: Preps Passed_Float for steps 3-5 and holds exponent
   -- Parameters: Passed_Float (raw input)
   -- Local vars: Exp_Holder (Integer) for holding iterations of the
   --                while loop in the body
   --             Passed_Float_Math (Float) holds copy of input for
   --                enabling modification
   -- Precondition: Parameter is valid
   -- Postcondition: Prints Binary Sci version of Passed_Float,
   --            Prints Calculated FP representation of Passed_Float
   ------------------------------------------------------------------

   PROCEDURE Bin_Sci (Passed_Float : IN Float) IS

      ------------------------------------------------------------------
      -- Purpose: Calculates/prints exponent portion of FP representation
      -- Parameters: Zero_Check (special condition param),
      --             Exponent (from body of Bin_Sci
      -- Local vars: Convert_Me (Integer) adds/holds second param +127
      --             Hold_Conversion (Array) holds 8 digit binary
      --                 version of Convert_Me
      -- Precondition: Parameter is valid
      -- Postcondition: Prints out exponent portion of FP representation
      ------------------------------------------------------------------

      PROCEDURE Binary_Magic (Zero_Check : IN Integer;
            Exponent : IN Integer) IS

         Convert_Me : Integer := Exponent + 127;
         Hold_Conversion : ARRAY(0 .. 7) OF Integer RANGE 0 .. 1;

      BEGIN

         FOR I IN REVERSE 0 .. 7 LOOP

            -- Normally, this would print out 0111 1111 (127) for 0 so I
            -- installed the zero_check param for this purpose
            -- meaning if (0, ?) is passed, all zeroes will
            -- print in the array

            IF Convert_Me >= (2**(I)) AND Zero_Check /= 0 THEN
               Convert_Me := Convert_Me - (2**(I));
               Hold_Conversion(7-I) := 1;
               Put(Hold_Conversion(7-I), Width => 0);

            ELSE
               Hold_Conversion(7-I) := 0;
               Put(Hold_Conversion(7-I), Width => 0);

            END IF;

            IF I mod 4 = 0 THEN
               Put(" ");
            END IF;

         END LOOP;

      END Binary_Magic;

      ------------------------------------------------------------------
      -- Purpose: Handles mantissa calculation
      -- Parameters: Passed_Float (significand)
      -- Local vars: Significand (Float)
      --             Significand_Math (Float) holds Significand - 1 so
      --                 mantissa calculation can take place
      --             Math (Float) holds powers of 2
      --             Hold_Mantissa (Array) holds the mantissa calculation
      -- Precondition: Parameter is >= 1.0 and < 2.0
      -- Postcondition: 23 digit mantissa
      ------------------------------------------------------------------

      PROCEDURE Binary_Array_Printer (Passed_Float : IN Float) IS
         Significand : Float := Passed_Float;
         Significand_Math : Float := Passed_Float - 1.0;
         Math : Float;
         Hold_Mantissa: ARRAY(1 .. 23) OF Integer RANGE 0 .. 1;

      BEGIN

         FOR I IN 1 .. 23 LOOP

            Math := 0.5**I;

            IF Significand_Math >= Math THEN

               Significand_Math := Significand_Math - Math;
               Hold_Mantissa(I) := 1;
               Put(Hold_Mantissa(I), width => 0);

            ELSE

               Hold_Mantissa(I) := 0;
               Put(Hold_Mantissa(I), width => 0);

            END IF;

            IF I Mod 4 = 0 THEN

               Put(" ");

            END IF;

         END LOOP;

      END Binary_Array_Printer;


   ------------ Continuing with declarations and body of Bin_Sci

   Exp_Holder : Integer := 0;
   Passed_Float_Math : Float := Passed_Float;

   BEGIN

      IF Passed_Float_Math < 0.0 THEN

         -- Rest of Bin_Sci can't function on a negative so
         --   the input is multiplied by negative one to make a positive
         --   for exponent and mantissa calculations

         Passed_Float_Math := Passed_Float_Math * (-1.0);

      END IF;

      WHILE Passed_Float_Math < 1.0 OR Passed_Float_Math >= 2.0 LOOP

         -- The secondary checks in this WHILE for IF and ELSIF keep
         -- 0.0 out of those statements

         IF Passed_Float_Math < 1.0 AND Passed_Float_Math /= 0.0 THEN

            Passed_Float_Math := Passed_Float_Math * 2.0;
            Exp_Holder := Exp_Holder - 1;

         ELSIF Passed_Float_Math >= 2.0 AND Passed_Float_Math /= 0.0 THEN

            Passed_Float_Math := Passed_Float_Math / 2.0;
            Exp_Holder := Exp_Holder + 1;

         ELSE

            EXIT; -- So the WHILE LOOP doesn't go infinite on 0.0

         END IF;

      END LOOP;

      ---Print step 3---

      Put("Binary scientific notation:      ");

      IF Passed_Float < 0.0 THEN
         Put("-1.");

      ELSIF Passed_Float = 0.0 THEN
         Put("0.");

      ELSE
         Put("1.");

      END IF;

      Binary_Array_Printer(Passed_Float_Math); Put(" * 2**");
      Put(Exp_Holder, Width => 0);
      New_Line;
      ---End step 3---

      ---Print step 4---
      Put("Calculated FP Representation:    ");
      IF Passed_Float < 0.0 THEN
         Put("1");
      ELSE
         Put("0");
      END IF;
      Put(" ");

      IF Passed_Float = 0.0 THEN

         Binary_Magic(0, Exp_Holder);

      ELSE

         Binary_Magic(1, Exp_Holder);

      END IF;

         Binary_Array_Printer(Passed_Float_Math);
         New_Line;
      ---End step 4---

   END Bin_Sci;

   ------------------------------------------------------------------
   -- Purpose: Passes the raw input to the various procedures via calls
   -- Parameters: Passed_Float (raw input)
   -- Local vars: F (Float) holds a copy of input in case it needs to be
   --               modified
   -- Precondition: Parameter is valid
   -- Postcondition: Prints all output not handled by procedures
   --    themselves
   ------------------------------------------------------------------

   PROCEDURE Printer (Passed_Float: IN Float) IS

      F: Float := Passed_Float;

   BEGIN

      Put("As entered:");
      Put(F, Fore => 23, Aft => 23, Exp => 0); -- Float with 23 places
      New_Line;

      Put("Decimal scientific notation:");
      Put(F, Fore => 6); -- Supposedly prints F in scientific notation?
      New_Line;

      Bin_Sci(F); -- Procedure call to get binary, sig, and exp
      Put("Conversion to bit array:         ");

      FOR I IN REVERSE 1 .. 32 LOOP
         Fill_Array(F, I);
         IF I mod 4 =0 THEN
            Put(" ");
         END IF;
      END LOOP;

      New_Line;

      Unsigned_Float(F);

      New_Line;
      New_Line;

   END Printer;

   ------------------------------------------------------------------

   F: Float;

BEGIN

   WHILE NOT End_Of_File LOOP

      BEGIN

         Get(F);
         Printer(F);

      EXCEPTION

         WHEN Data_Error =>
            Put("Incorrect input type, please try again");
            Skip_Line; New_Line;

         WHEN Constraint_Error =>
            Put("Out of bounds, please try again");
            Skip_Line; New_Line;

      END;

   END LOOP;

END Floating;
