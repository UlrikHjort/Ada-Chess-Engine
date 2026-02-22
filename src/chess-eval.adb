-- ***************************************************************************
--                      Chess - Position Evaluator Body
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

with Chess.Board;  use Chess.Board;
with Chess.Moves;  use Chess.Moves;

package body Chess.Eval is

   -- =========================================================================
   -- Material values (centipawns)
   -- =========================================================================
   function Material_Value (Kind : Piece_Kind) return Integer is
   begin
      case Kind is
         when Pawn   => return 100;
         when Knight => return 320;
         when Bishop => return 330;
         when Rook   => return 500;
         when Queen  => return 900;
         when King   => return 0;   -- King has no material value
      end case;
   end Material_Value;

   -- =========================================================================
   -- Piece-Square Tables (from White's perspective; rank 1 = White back rank)
   -- Values encourage good piece placement: knights and bishops in the center,
   -- rooks on open files, pawns advanced, king safe in the corner/castled.
   -- =========================================================================

   -- Piece-square tables are stored [File_A..File_H][Rank_1..Rank_8]
   -- Values from White's point of view. Black mirrors ranks (rank 8 -> rank 1).
   type PST is array (File_Type, Rank_Type) of Integer;

   Pawn_PST : constant PST :=
     (File_A => (0, 5,  5, 0, 5, 10, 50, 0),
      File_B => (0, 10, -5, 0, 5, 10, 50, 0),
      File_C => (0, 10,-10, 0,10, 20, 50, 0),
      File_D => (0, -5, 0, 20,25, 30, 50, 0),
      File_E => (0, -5, 0, 20,25, 30, 50, 0),
      File_F => (0, 10,-10, 0,10, 20, 50, 0),
      File_G => (0, 10, -5, 0, 5, 10, 50, 0),
      File_H => (0,  5,  5, 0, 5, 10, 50, 0));

   Knight_PST : constant PST :=
     (File_A => (-50,-40,-30,-30,-30,-30,-40,-50),
      File_B => (-40,-20,  0,  5,  5,  0,-20,-40),
      File_C => (-30,  5, 10, 15, 15, 10,  5,-30),
      File_D => (-30,  0, 15, 20, 20, 15,  0,-30),
      File_E => (-30,  0, 15, 20, 20, 15,  0,-30),
      File_F => (-30,  5, 10, 15, 15, 10,  5,-30),
      File_G => (-40,-20,  0,  5,  5,  0,-20,-40),
      File_H => (-50,-40,-30,-30,-30,-30,-40,-50));

   Bishop_PST : constant PST :=
     (File_A => (-20,-10,-10,-10,-10,-10,-10,-20),
      File_B => (-10,  5,  0,  0,  0,  0,  5,-10),
      File_C => (-10, 10, 10, 10, 10, 10, 10,-10),
      File_D => (-10,  0, 10, 10, 10, 10,  0,-10),
      File_E => (-10,  0, 10, 10, 10, 10,  0,-10),
      File_F => (-10, 10, 10, 10, 10, 10, 10,-10),
      File_G => (-10,  5,  0,  0,  0,  0,  5,-10),
      File_H => (-20,-10,-10,-10,-10,-10,-10,-20));

   Rook_PST : constant PST :=
     (File_A => ( 0,  -5, -5, -5, -5, -5,  5,  0),
      File_B => ( 0,   0,  0,  0,  0,  0, 10,  0),
      File_C => ( 0,   0,  0,  0,  0,  0, 10,  0),
      File_D => ( 5,   0,  0,  0,  0,  0, 10,  0),
      File_E => ( 5,   0,  0,  0,  0,  0, 10,  0),
      File_F => ( 0,   0,  0,  0,  0,  0, 10,  0),
      File_G => ( 0,   0,  0,  0,  0,  0, 10,  0),
      File_H => ( 0,  -5, -5, -5, -5, -5,  5,  0));

   Queen_PST : constant PST :=
     (File_A => (-20,-10,-10, -5, -5,-10,-10,-20),
      File_B => (-10,  0,  5,  0,  0,  0,  0,-10),
      File_C => (-10,  5,  5,  5,  5,  5,  0,-10),
      File_D => (  0,  0,  5,  5,  5,  5,  0, -5),
      File_E => ( -5,  0,  5,  5,  5,  5,  0, -5),
      File_F => (-10,  0,  5,  5,  5,  5,  0,-10),
      File_G => (-10,  0,  0,  0,  0,  0,  0,-10),
      File_H => (-20,-10,-10, -5, -5,-10,-10,-20));

   -- King: encouraged to castle and stay on back rank in middlegame
   King_PST : constant PST :=
     (File_A => ( 20, 30, 10,  0,  0, 10, 30, 20),
      File_B => ( 20, 20,  0,  0,  0,  0, 20, 20),
      File_C => (-10,-20,-20,-20,-20,-20,-20,-10),
      File_D => (-20,-30,-30,-40,-40,-30,-30,-20),
      File_E => (-30,-40,-40,-50,-50,-40,-40,-30),
      File_F => (-30,-40,-40,-50,-50,-40,-40,-30),
      File_G => ( 20, 20,  0,  0,  0,  0, 20, 20),
      File_H => ( 20, 30, 10,  0,  0, 10, 30, 20));

   -- Lookup a PST value for a piece on a given square, adjusted for color.
   -- White uses the table as-is (rank 1 = back row).
   -- Black mirrors vertically (rank 8 = Black's back row).
   function PST_Score (Table  : PST;
                       Square : Square_Type;
                       Color  : Color_Type) return Integer is
      Rank : constant Rank_Type :=
        (if Color = White then Square.Rank
         else Rank_Type(9 - Integer(Square.Rank)));
   begin
      return Table(Square.File, Rank);
   end PST_Score;

   -- =========================================================================
   -- Main evaluation components
   -- =========================================================================

   -- Material + piece-square score for one side
   function Side_Score (State : Game_State; Color : Color_Type) return Integer is
      Score : Integer := 0;
      Sq    : Board_Square;
   begin
      for F in File_Type loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then Sq.Piece.Color = Color then
               Score := Score + Material_Value (Sq.Piece.Kind);
               Score := Score + PST_Score (
                  (case Sq.Piece.Kind is
                      when Pawn   => Pawn_PST,
                      when Knight => Knight_PST,
                      when Bishop => Bishop_PST,
                      when Rook   => Rook_PST,
                      when Queen  => Queen_PST,
                      when King   => King_PST),
                  (F, R), Color);
            end if;
         end loop;
      end loop;
      return Score;
   end Side_Score;

   -- Mobility bonus: count legal moves available (scaled to centipawns)
   function Mobility_Score (State : Game_State; Color : Color_Type) return Integer is
      Moves : constant Move_List :=
                Generate_All_Legal_Moves (State, Color);
   begin
      -- Each legal move is worth ~3 centipawns
      return Integer(Moves.Length) * 3;
   end Mobility_Score;

   -- Bishop pair bonus: having both bishops is worth ~50 cp
   function Bishop_Pair_Bonus (State : Game_State; Color : Color_Type) return Integer is
      Count : Natural := 0;
      Sq    : Board_Square;
   begin
      for F in File_Type loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then
               Sq.Piece.Color = Color and then
               Sq.Piece.Kind = Bishop then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return (if Count >= 2 then 50 else 0);
   end Bishop_Pair_Bonus;

   -- =========================================================================
   -- Pawn structure evaluation
   --
   -- Passed pawn:  no enemy pawns block or control the squares ahead.
   --   Bonus scales with how far the pawn has advanced.
   -- Doubled pawn: two or more friendly pawns on the same file → penalty.
   -- Isolated pawn: no friendly pawn on adjacent files → penalty.
   -- =========================================================================
   function Is_Passed_Pawn (State : Game_State;
                            Color : Color_Type;
                            File  : File_Type;
                            Rank  : Rank_Type) return Boolean is
      Enemy : constant Color_Type :=
                (if Color = White then Black else White);
      -- Direction the pawn advances
      Dir   : constant Integer := (if Color = White then 1 else -1);
   begin
      -- Check the pawn's file and adjacent files for enemy pawns ahead
      for DF in Integer range -1 .. 1 loop
         declare
            NF_Int : constant Integer := Integer (File_Type'Pos (File)) + DF;
         begin
            if NF_Int >= File_Type'Pos (File_Type'First) and then
               NF_Int <= File_Type'Pos (File_Type'Last)
            then
               declare
                  NF : constant File_Type := File_Type'Val (NF_Int);
                  R  : Rank_Type := Rank;
               begin
                  -- Walk forward until the edge of the board
                  loop
                     declare
                        Next_R : constant Integer :=
                                   Rank_Type'Pos (R) + Dir;
                     begin
                        exit when Next_R < Rank_Type'Pos (Rank_Type'First)
                               or else Next_R > Rank_Type'Pos (Rank_Type'Last);
                        R := Rank_Type'Val (Next_R);
                     end;
                     declare
                        Sq : constant Board_Square := Get_Piece (State, (NF, R));
                     begin
                        if Sq.Content = Occupied and then
                           Sq.Piece.Color = Enemy and then
                           Sq.Piece.Kind = Pawn then
                           return False;  -- blocked by enemy pawn
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end;
      end loop;
      return True;
   end Is_Passed_Pawn;

   -- Passed-pawn rank bonus table (0-based: index = distance from own back rank)
   -- index 0 = rank 2 for white, rank 7 for black (one step from start)
   Passed_Bonus : constant array (0 .. 7) of Integer :=
     (0, 5, 10, 20, 40, 70, 110, 0);  -- index 7 never used (promoted)

   function Pawn_Structure_Score (State : Game_State;
                                  Color : Color_Type) return Integer is
      Score       : Integer := 0;
      File_Count  : array (File_Type) of Natural := (others => 0);
      Sq          : Board_Square;
   begin
      -- Count pawns per file and score each pawn
      for F in File_Type loop
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then
               Sq.Piece.Color = Color and then
               Sq.Piece.Kind = Pawn
            then
               File_Count (F) := File_Count (F) + 1;

               -- Passed pawn bonus
               if Is_Passed_Pawn (State, Color, F, R) then
                  declare
                     Advance : constant Integer :=
                       (if Color = White
                        then Rank_Type'Pos (R) - Rank_Type'Pos (Rank_Type'First)
                        else Rank_Type'Pos (Rank_Type'Last) - Rank_Type'Pos (R));
                  begin
                     Score := Score + Passed_Bonus (Advance);
                  end;
               end if;
            end if;
         end loop;
      end loop;

      -- Doubled pawn penalty (-20 cp per extra pawn on same file)
      for F in File_Type loop
         if File_Count (F) > 1 then
            Score := Score - 20 * (File_Count (F) - 1);
         end if;
      end loop;

      -- Isolated pawn penalty (-15 cp per pawn with no friendly neighbours)
      for F in File_Type loop
         if File_Count (F) > 0 then
            declare
               F_Int     : constant Integer := File_Type'Pos (F);
               Has_Left  : Boolean := False;
               Has_Right : Boolean := False;
            begin
               if F_Int > File_Type'Pos (File_Type'First) then
                  Has_Left := File_Count (File_Type'Val (F_Int - 1)) > 0;
               end if;
               if F_Int < File_Type'Pos (File_Type'Last) then
                  Has_Right := File_Count (File_Type'Val (F_Int + 1)) > 0;
               end if;
               if not Has_Left and then not Has_Right then
                  Score := Score - 15 * File_Count (F);
               end if;
            end;
         end if;
      end loop;

      return Score;
   end Pawn_Structure_Score;

   -- =========================================================================
   -- King safety
   --
   -- Simple pawn-shield bonus: award points for friendly pawns on the two
   -- ranks directly in front of the king (f2/g2/h2 for a castled White king
   -- on g1, etc.).  Also subtract a penalty for each open file adjacent to
   -- the king (enemy rooks/queens love open files).
   -- =========================================================================
   function King_Safety_Score (State : Game_State;
                               Color : Color_Type) return Integer is
      Score    : Integer := 0;
      Sq       : Board_Square;
      Enemy    : constant Color_Type :=
                   (if Color = White then Black else White);
      King_Sq  : Square_Type;
      Found    : Boolean := False;
      King_File_Int : Integer;
   begin
      -- Find the king
      for F in File_Type loop
         exit when Found;
         for R in Rank_Type loop
            Sq := Get_Piece (State, (F, R));
            if Sq.Content = Occupied and then
               Sq.Piece.Color = Color and then
               Sq.Piece.Kind = King
            then
               King_Sq := (F, R);
               Found   := True;
               exit;
            end if;
         end loop;
      end loop;

      if not Found then
         return 0;
      end if;

      King_File_Int := File_Type'Pos (King_Sq.File);

      -- Pawn-shield: check 3 files around the king, 1 and 2 ranks ahead
      for DF in Integer range -1 .. 1 loop
         declare
            SF_Int : constant Integer := King_File_Int + DF;
         begin
            if SF_Int >= File_Type'Pos (File_Type'First) and then
               SF_Int <= File_Type'Pos (File_Type'Last)
            then
               declare
                  SF  : constant File_Type  := File_Type'Val (SF_Int);
                  Dir : constant Integer    := (if Color = White then 1 else -1);
                  R1_Int : constant Integer :=
                    Rank_Type'Pos (King_Sq.Rank) + Dir;
                  R2_Int : constant Integer :=
                    Rank_Type'Pos (King_Sq.Rank) + 2 * Dir;
               begin
                  -- +10 for pawn one square ahead, +5 two squares ahead
                  if R1_Int >= Rank_Type'Pos (Rank_Type'First) and then
                     R1_Int <= Rank_Type'Pos (Rank_Type'Last)
                  then
                     declare
                        S1 : constant Board_Square :=
                          Get_Piece (State, (SF, Rank_Type'Val (R1_Int)));
                     begin
                        if S1.Content = Occupied and then
                           S1.Piece.Color = Color and then
                           S1.Piece.Kind = Pawn
                        then
                           Score := Score + 10;
                        end if;
                     end;
                  end if;
                  if R2_Int >= Rank_Type'Pos (Rank_Type'First) and then
                     R2_Int <= Rank_Type'Pos (Rank_Type'Last)
                  then
                     declare
                        S2 : constant Board_Square :=
                          Get_Piece (State, (SF, Rank_Type'Val (R2_Int)));
                     begin
                        if S2.Content = Occupied and then
                           S2.Piece.Color = Color and then
                           S2.Piece.Kind = Pawn
                        then
                           Score := Score + 5;
                        end if;
                     end;
                  end if;

                  -- Open file near king: check if enemy has rook/queen on this file
                  declare
                     Has_Friendly_Pawn : Boolean := False;
                     Has_Enemy_Major   : Boolean := False;
                  begin
                     for R in Rank_Type loop
                        declare
                           FSq : constant Board_Square :=
                             Get_Piece (State, (SF, R));
                        begin
                           if FSq.Content = Occupied then
                              if FSq.Piece.Color = Color and then
                                 FSq.Piece.Kind = Pawn
                              then
                                 Has_Friendly_Pawn := True;
                              end if;
                              if FSq.Piece.Color = Enemy and then
                                 (FSq.Piece.Kind = Rook or else
                                  FSq.Piece.Kind = Queen)
                              then
                                 Has_Enemy_Major := True;
                              end if;
                           end if;
                        end;
                     end loop;
                     if not Has_Friendly_Pawn and then Has_Enemy_Major then
                        Score := Score - 25;  -- open file with enemy major piece
                     end if;
                  end;
               end;
            end if;
         end;
      end loop;

      return Score;
   end King_Safety_Score;

   -- =========================================================================
   -- Public functions
   -- =========================================================================

   function Evaluate (State : Game_State) return Centipawns is
      White_Score : Integer;
      Black_Score : Integer;
   begin
      White_Score := Side_Score         (State, White) +
                     Mobility_Score     (State, White) +
                     Bishop_Pair_Bonus  (State, White) +
                     Pawn_Structure_Score (State, White) +
                     King_Safety_Score  (State, White);

      Black_Score := Side_Score         (State, Black) +
                     Mobility_Score     (State, Black) +
                     Bishop_Pair_Bonus  (State, Black) +
                     Pawn_Structure_Score (State, Black) +
                     King_Safety_Score  (State, Black);

      return White_Score - Black_Score;
   end Evaluate;

   function Evaluate_Verbose (State : Game_State) return String is
      White_Mat  : constant Integer := Side_Score           (State, White);
      Black_Mat  : constant Integer := Side_Score           (State, Black);
      White_Mob  : constant Integer := Mobility_Score       (State, White);
      Black_Mob  : constant Integer := Mobility_Score       (State, Black);
      White_BP   : constant Integer := Bishop_Pair_Bonus    (State, White);
      Black_BP   : constant Integer := Bishop_Pair_Bonus    (State, Black);
      White_PS   : constant Integer := Pawn_Structure_Score (State, White);
      Black_PS   : constant Integer := Pawn_Structure_Score (State, Black);
      White_KS   : constant Integer := King_Safety_Score    (State, White);
      Black_KS   : constant Integer := King_Safety_Score    (State, Black);
      Total      : constant Integer :=
        (White_Mat + White_Mob + White_BP + White_PS + White_KS)
      - (Black_Mat + Black_Mob + Black_BP + Black_PS + Black_KS);

      function Img (N : Integer) return String is
         S : constant String := Integer'Image(N);
      begin
         return (if N >= 0 then "+" & S(S'First + 1 .. S'Last) else S);
      end Img;
   begin
      return
        "Material:   White=" & Img(White_Mat) & "  Black=" & Img(Black_Mat) & ASCII.LF &
        "Mobility:   White=" & Img(White_Mob) & "  Black=" & Img(Black_Mob) & ASCII.LF &
        "Bshp pair:  White=" & Img(White_BP)  & "  Black=" & Img(Black_BP)  & ASCII.LF &
        "Pawn struc: White=" & Img(White_PS)  & "  Black=" & Img(Black_PS)  & ASCII.LF &
        "King sfty:  White=" & Img(White_KS)  & "  Black=" & Img(Black_KS)  & ASCII.LF &
        "Total (cp):" & Img(Total) &
        (if Total > 50 then "  (White is better)" elsif Total < -50 then
           "  (Black is better)" else "  (roughly equal)");
   end Evaluate_Verbose;

end Chess.Eval;
