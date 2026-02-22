-- ***************************************************************************
--                      Chess - Bitboard Foundation Body
--
--           Copyright (C) 2026 By Ulrik Hørlyk Hjort
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-- ***************************************************************************

with Chess.Board;   use Chess.Board;
with Ada.Text_IO;   use Ada.Text_IO;
with Interfaces;    use Interfaces;

package body Chess.Bitboard is

   -- Disambiguate Chess.Types.File_Type from Ada.Text_IO.File_Type
   subtype Board_File is Chess.Types.File_Type;

   -- =========================================================================
   -- Low-level helpers
   -- =========================================================================

   function Square_To_Bit (Sq : Square_Type) return Natural is
      File_Idx : constant Natural :=
        Board_File'Pos (Sq.File);  -- File_A=0 .. File_H=7
      Rank_Idx : constant Natural :=
        Natural (Sq.Rank) - 1;    -- Rank 1..8 → 0..7
   begin
      return Rank_Idx * 8 + File_Idx;
   end Square_To_Bit;

   function Bit (Index : Natural) return Bitboard is
   begin
      return Shift_Left (1, Index);
   end Bit;

   function Set_Square (BB : Bitboard; Sq : Square_Type) return Bitboard is
   begin
      return BB or Bit (Square_To_Bit (Sq));
   end Set_Square;

   function Clear_Square (BB : Bitboard; Sq : Square_Type) return Bitboard is
   begin
      return BB and (not Bit (Square_To_Bit (Sq)));
   end Clear_Square;

   function Test_Square (BB : Bitboard; Sq : Square_Type) return Boolean is
   begin
      return (BB and Bit (Square_To_Bit (Sq))) /= 0;
   end Test_Square;

   -- Kernighan bit-counting algorithm
   function Popcount (BB : Bitboard) return Natural is
      B : Bitboard := BB;
      N : Natural  := 0;
   begin
      while B /= 0 loop
         B := B and (B - 1);
         N := N + 1;
      end loop;
      return N;
   end Popcount;

   function LSB (BB : Bitboard) return Natural is
      B : Bitboard := BB;
      N : Natural  := 0;
   begin
      while (B and 1) = 0 loop
         B := Shift_Right (B, 1);
         N := N + 1;
      end loop;
      return N;
   end LSB;

   function Pop_LSB (BB : in out Bitboard) return Natural is
      Index : constant Natural := LSB (BB);
   begin
      BB := BB and (BB - 1);  -- clear lowest set bit
      return Index;
   end Pop_LSB;

   -- =========================================================================
   -- Pre-computed attack table initialisation
   -- =========================================================================

   -- Build knight attack bitboard for a given square index
   function Compute_Knight_Attacks (Sq : Natural) return Bitboard is
      BB     : Bitboard := 0;
      Rank   : constant Integer := Sq / 8;   -- 0-based
      File_I : constant Integer := Sq mod 8; -- 0-based
      Deltas : constant array (1 .. 8, 1 .. 2) of Integer :=
        ((2, 1), (2, -1), (-2, 1), (-2, -1),
         (1, 2), (1, -2), (-1, 2), (-1, -2));
      NR, NF : Integer;
   begin
      for D in 1 .. 8 loop
         NR := Rank   + Deltas (D, 1);
         NF := File_I + Deltas (D, 2);
         if NR in 0 .. 7 and then NF in 0 .. 7 then
            BB := BB or Bit (NR * 8 + NF);
         end if;
      end loop;
      return BB;
   end Compute_Knight_Attacks;

   -- Build king attack bitboard for a given square index
   function Compute_King_Attacks (Sq : Natural) return Bitboard is
      BB     : Bitboard := 0;
      Rank   : constant Integer := Sq / 8;
      File_I : constant Integer := Sq mod 8;
      NR, NF : Integer;
   begin
      for DR in -1 .. 1 loop
         for DF in -1 .. 1 loop
            if DR /= 0 or DF /= 0 then
               NR := Rank   + DR;
               NF := File_I + DF;
               if NR in 0 .. 7 and then NF in 0 .. 7 then
                  BB := BB or Bit (NR * 8 + NF);
               end if;
            end if;
         end loop;
      end loop;
      return BB;
   end Compute_King_Attacks;

   -- Build pawn attack bitboard for a given square index and color
   function Compute_Pawn_Attacks (Sq : Natural; Color : Color_Type) return Bitboard is
      BB     : Bitboard := 0;
      Rank   : constant Integer := Sq / 8;
      File_I : constant Integer := Sq mod 8;
      NR     : constant Integer := Rank + (if Color = White then 1 else -1);
   begin
      if NR in 0 .. 7 then
         if File_I - 1 in 0 .. 7 then
            BB := BB or Bit (NR * 8 + File_I - 1);
         end if;
         if File_I + 1 in 0 .. 7 then
            BB := BB or Bit (NR * 8 + File_I + 1);
         end if;
      end if;
      return BB;
   end Compute_Pawn_Attacks;

   procedure Initialize_Bitboards is
   begin
      for Sq in 0 .. 63 loop
         Knight_Attacks (Sq) := Compute_Knight_Attacks (Sq);
         King_Attacks   (Sq) := Compute_King_Attacks   (Sq);
         Pawn_Attacks (White, Sq) := Compute_Pawn_Attacks (Sq, White);
         Pawn_Attacks (Black, Sq) := Compute_Pawn_Attacks (Sq, Black);
      end loop;
   end Initialize_Bitboards;

   -- =========================================================================
   -- Sliding piece attacks using classical ray-casting (Kogge-Stone style)
   -- =========================================================================

   -- Slide along a direction (dr, df) until blocked; return attacked squares
   function Slide (Sq : Natural; Occupied : Bitboard;
                   DR : Integer; DF : Integer) return Bitboard is
      BB     : Bitboard := 0;
      Rank   : Integer  := Sq / 8 + DR;
      File_I : Integer  := Sq mod 8 + DF;
      Idx    : Natural;
   begin
      while Rank in 0 .. 7 and then File_I in 0 .. 7 loop
         Idx := Natural (Rank) * 8 + Natural (File_I);
         BB  := BB or Bit (Idx);
         exit when (Occupied and Bit (Idx)) /= 0;  -- stop at first piece hit
         Rank   := Rank   + DR;
         File_I := File_I + DF;
      end loop;
      return BB;
   end Slide;

   function Rook_Attacks (Sq : Natural; Occupied : Bitboard) return Bitboard is
   begin
      return Slide (Sq, Occupied,  1,  0) or
             Slide (Sq, Occupied, -1,  0) or
             Slide (Sq, Occupied,  0,  1) or
             Slide (Sq, Occupied,  0, -1);
   end Rook_Attacks;

   function Bishop_Attacks (Sq : Natural; Occupied : Bitboard) return Bitboard is
   begin
      return Slide (Sq, Occupied,  1,  1) or
             Slide (Sq, Occupied,  1, -1) or
             Slide (Sq, Occupied, -1,  1) or
             Slide (Sq, Occupied, -1, -1);
   end Bishop_Attacks;

   function Queen_Attacks (Sq : Natural; Occupied : Bitboard) return Bitboard is
   begin
      return Rook_Attacks (Sq, Occupied) or Bishop_Attacks (Sq, Occupied);
   end Queen_Attacks;

   -- =========================================================================
   -- Conversion from Game_State to bitboards
   -- =========================================================================

   function Occupancy (State : Game_State) return Bitboard is
      BB : Bitboard := 0;
      Sq : Board_Square;
   begin
      for F in Board_File loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied then
               BB := Set_Square (BB, (F, R));
            end if;
         end loop;
      end loop;
      return BB;
   end Occupancy;

   function White_Pieces (State : Game_State) return Bitboard is
      BB : Bitboard := 0;
      Sq : Board_Square;
   begin
      for F in Board_File loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then Sq.Piece.Color = White then
               BB := Set_Square (BB, (F, R));
            end if;
         end loop;
      end loop;
      return BB;
   end White_Pieces;

   function Black_Pieces (State : Game_State) return Bitboard is
      BB : Bitboard := 0;
      Sq : Board_Square;
   begin
      for F in Board_File loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then Sq.Piece.Color = Black then
               BB := Set_Square (BB, (F, R));
            end if;
         end loop;
      end loop;
      return BB;
   end Black_Pieces;

   function Pieces_Of_Kind (State : Game_State;
                             Color : Color_Type;
                             Kind  : Piece_Kind) return Bitboard is
      BB : Bitboard := 0;
      Sq : Board_Square;
   begin
      for F in Board_File loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then
               Sq.Piece.Color = Color and then
               Sq.Piece.Kind  = Kind  then
               BB := Set_Square (BB, (F, R));
            end if;
         end loop;
      end loop;
      return BB;
   end Pieces_Of_Kind;

   -- =========================================================================
   -- Debug printer
   -- =========================================================================

   procedure Print_Bitboard (BB : Bitboard; Label : String := "") is
   begin
      if Label /= "" then
         Put_Line (Label);
      end if;
      for R in reverse 0 .. 7 loop
         Put (Rank_Type'Image (Rank_Type (R + 1)) & " ");
         for F in 0 .. 7 loop
            if (BB and Bit (R * 8 + F)) /= 0 then
               Put (" X");
            else
               Put (" .");
            end if;
         end loop;
         New_Line;
      end loop;
      Put_Line ("   a b c d e f g h");
   end Print_Bitboard;

   -- =========================================================================
   -- Is_Square_Attacked_BB
   --
   -- Tests whether 'Square' is attacked by any piece belonging to 'By_Color'.
   -- The idea: from Square's perspective, cast rays/attacks *as if* we were
   -- each piece type.  If that attack bitboard intersects the actual pieces of
   -- that type owned by By_Color, then the square is attacked.
   --
   -- Example: to check knight attacks on Square, compute Knight_Attacks[sq]
   -- and AND with the bitboard of enemy knights.  If non-zero, a knight
   -- attacks the square.
   -- =========================================================================
   function Is_Square_Attacked_BB (State    : Game_State;
                                   Square   : Square_Type;
                                   By_Color : Color_Type) return Boolean
   is
      Sq_Bit  : constant Natural  := Square_To_Bit (Square);
      Occ     : constant Bitboard := Occupancy (State);

      function Pieces (Kind : Piece_Kind) return Bitboard is
      begin
         return Pieces_Of_Kind (State, By_Color, Kind);
      end Pieces;
   begin
      -- Knight attacks
      if (Knight_Attacks (Sq_Bit) and Pieces (Knight)) /= 0 then
         return True;
      end if;

      -- King attacks
      if (King_Attacks (Sq_Bit) and Pieces (King)) /= 0 then
         return True;
      end if;

      -- Pawn attacks: pawns attack diagonally forward.
      -- To find if an enemy pawn attacks Square, use the *opposite* color's
      -- pawn attack table from Square (i.e. where a pawn of our color would
      -- attack from Square).
      declare
         Own_Color : constant Color_Type :=
           (if By_Color = White then Black else White);
      begin
         if (Pawn_Attacks (Own_Color, Sq_Bit) and Pieces (Pawn)) /= 0 then
            return True;
         end if;
      end;

      -- Rook / Queen (straight rays)
      declare
         RA : constant Bitboard := Rook_Attacks (Sq_Bit, Occ);
      begin
         if (RA and (Pieces (Rook) or Pieces (Queen))) /= 0 then
            return True;
         end if;
      end;

      -- Bishop / Queen (diagonal rays)
      declare
         BA : constant Bitboard := Bishop_Attacks (Sq_Bit, Occ);
      begin
         if (BA and (Pieces (Bishop) or Pieces (Queen))) /= 0 then
            return True;
         end if;
      end;

      return False;
   end Is_Square_Attacked_BB;

end Chess.Bitboard;
