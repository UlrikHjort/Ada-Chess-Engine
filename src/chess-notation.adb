-- ***************************************************************************
--                      Chess - Notation Module Body
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

with Chess.Board; use Chess.Board;
with Chess.Moves; use Chess.Moves;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Chess.Notation is

   function Parse_Square (S : String) return Square_Type is
   begin
      if S'Length /= 2 then
         raise Notation_Error with "Invalid square notation: " & S;
      end if;
      
      declare
         File_Char : constant Character := To_Lower (S (S'First));
         Rank_Char : constant Character := S (S'First + 1);
      begin
         if File_Char not in 'a' .. 'h' or Rank_Char not in '1' .. '8' then
            raise Notation_Error with "Invalid square: " & S;
         end if;
         
         return (File => Char_To_File (File_Char),
                 Rank => Rank_Type'Value (Rank_Char & ""));
      end;
   end Parse_Square;

   function Char_To_Piece_Kind (C : Character) return Piece_Kind is
   begin
      case To_Upper (C) is
         when 'K' => return King;
         when 'Q' => return Queen;
         when 'R' => return Rook;
         when 'B' => return Bishop;
         when 'N' => return Knight;
         when 'P' => return Pawn;
         when others => raise Notation_Error with "Invalid piece character: " & C;
      end case;
   end Char_To_Piece_Kind;

   function Piece_Kind_To_Char (Kind : Piece_Kind) return Character is
   begin
      case Kind is
         when King   => return 'K';
         when Queen  => return 'Q';
         when Rook   => return 'R';
         when Bishop => return 'B';
         when Knight => return 'N';
         when Pawn   => return ' ';  -- Pawns are implicit
      end case;
   end Piece_Kind_To_Char;

   function Parse_Move (State    : Game_State;
                       Notation : String) return Move_Type is
      Input : constant String := Trim (Notation, Ada.Strings.Both);
      Legal_Moves : constant Move_List := 
        Generate_All_Legal_Moves (State, State.Current_Player);
   begin
      if Input'Length = 0 then
         raise Notation_Error with "Empty move notation";
      end if;
      
      -- Castling
      if Input = "O-O" or Input = "0-0" or Input = "o-o" then
         for Move of Legal_Moves loop
            if Move.Flag = Castle_Kingside then
               return Move;
            end if;
         end loop;
         raise Notation_Error with "Kingside castling not legal";
      end if;
      
      if Input = "O-O-O" or Input = "0-0-0" or Input = "o-o-o" then
         for Move of Legal_Moves loop
            if Move.Flag = Castle_Queenside then
               return Move;
            end if;
         end loop;
         raise Notation_Error with "Queenside castling not legal";
      end if;
      
      -- Strip check/checkmate indicators
      declare
         Last_Idx : Natural := Input'Last;
      begin
         if Input'Length > 0 and then (Input (Input'Last) = '+' or Input (Input'Last) = '#') then
            Last_Idx := Input'Last - 1;
         end if;
         
         declare
            Clean : constant String := Input (Input'First .. Last_Idx);
         begin
         
         -- Handle pawn moves (e4, e8=Q, exd5)
         if Clean'Length >= 2 and then Clean (Clean'First) in 'a' .. 'h' and then
            Clean (Clean'First + 1) in '1' .. '8' then
            -- Simple pawn push like "e4"
            declare
               To_Square : Square_Type;
               Promotion_Piece : Piece_Kind := Queen;
               Idx : Positive := Clean'First;
            begin
               To_Square := Parse_Square (Clean (Idx .. Idx + 1));
               Idx := Idx + 2;
               
               -- Check for promotion
               if Idx <= Clean'Last then
                  if Clean (Idx) = '=' then
                     Idx := Idx + 1;
                  end if;
                  if Idx <= Clean'Last then
                     Promotion_Piece := Char_To_Piece_Kind (Clean (Idx));
                  end if;
               end if;
               
               -- Find matching pawn move to this square
               for Move of Legal_Moves loop
                  if Move.To = To_Square then
                     declare
                        From_Piece : constant Board_Square := 
                          Get_Piece (State, Move.From);
                     begin
                        if From_Piece.Content = Occupied and then
                           From_Piece.Piece.Kind = Pawn then
                           if Move.Flag = Promotion then
                              if Move.Promotion = Promotion_Piece then
                                 return Move;
                              end if;
                           else
                              return Move;
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
               
               raise Notation_Error with "No legal pawn move to: " & Clean;
            end;
         elsif Clean'Length >= 3 and then Clean (Clean'First) in 'a' .. 'h' and then
               Clean (Clean'First + 1) = 'x' then
            -- Pawn capture like "exd5"
            declare
               From_File : File_Type;
               To_Square : Square_Type;
               Is_Capture : Boolean := False;
               Promotion_Piece : Piece_Kind := Queen;
               Idx : Positive := Clean'First;
            begin
               From_File := Char_To_File (Clean (Idx));
               Idx := Idx + 1;
               
               -- Check for capture
               if Idx <= Clean'Last and then Clean (Idx) = 'x' then
                  Is_Capture := True;
                  Idx := Idx + 1;
               end if;
               
               -- Parse destination square
               if Idx + 1 > Clean'Last then
                  raise Notation_Error with "Incomplete move notation: " & Input;
               end if;
               
               To_Square := Parse_Square (Clean (Idx .. Idx + 1));
               Idx := Idx + 2;
               
               -- Check for promotion
               if Idx <= Clean'Last then
                  if Clean (Idx) = '=' then
                     Idx := Idx + 1;
                  end if;
                  if Idx <= Clean'Last then
                     Promotion_Piece := Char_To_Piece_Kind (Clean (Idx));
                  end if;
               end if;
               
               -- Find matching move
               for Move of Legal_Moves loop
                  if Move.From.File = From_File and then
                     Move.To = To_Square then
                     declare
                        From_Piece : constant Board_Square := 
                          Get_Piece (State, Move.From);
                     begin
                        if From_Piece.Content = Occupied and then
                           From_Piece.Piece.Kind = Pawn then
                           if Move.Flag = Promotion then
                              if Move.Promotion = Promotion_Piece then
                                 return Move;
                              end if;
                           else
                              return Move;
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
               
               raise Notation_Error with "No legal pawn move matches: " & Input;
            end;
         end if;
         
         -- Handle piece moves (Nf3, Qxd5, Rae1, N1f3)
         declare
            Piece : Piece_Kind;
            From_File : File_Type := File_Type'First;
            From_Rank : Rank_Type := Rank_Type'First;
            Has_From_File : Boolean := False;
            Has_From_Rank : Boolean := False;
            Is_Capture : Boolean := False;
            To_Square : Square_Type;
            Idx : Positive := Clean'First;
         begin
            -- Parse piece type
            Piece := Char_To_Piece_Kind (Clean (Idx));
            Idx := Idx + 1;
            
            -- Parse disambiguation and capture
            while Idx < Clean'Last loop
               if Clean (Idx) in 'a' .. 'h' and then
                  (Idx + 1 > Clean'Last or else Clean (Idx + 1) /= 'x') and then
                  (Idx + 1 <= Clean'Last and then Clean (Idx + 1) not in '1' .. '8') then
                  From_File := Char_To_File (Clean (Idx));
                  Has_From_File := True;
                  Idx := Idx + 1;
               elsif Clean (Idx) in '1' .. '8' and then
                     (Idx + 1 > Clean'Last or else Clean (Idx + 1) /= 'x') then
                  From_Rank := Rank_Type'Value (Clean (Idx) & "");
                  Has_From_Rank := True;
                  Idx := Idx + 1;
               elsif Clean (Idx) = 'x' then
                  Is_Capture := True;
                  Idx := Idx + 1;
                  exit;
               else
                  exit;
               end if;
            end loop;
            
            -- Parse destination
            if Idx + 1 > Clean'Last then
               raise Notation_Error with "Incomplete move notation: " & Input;
            end if;
            
            To_Square := Parse_Square (Clean (Idx .. Idx + 1));
            
            -- Find matching move
            for Move of Legal_Moves loop
               if Move.To = To_Square then
                  declare
                     From_Piece : constant Board_Square := 
                       Get_Piece (State, Move.From);
                  begin
                     if From_Piece.Content = Occupied and then
                        From_Piece.Piece.Kind = Piece then
                        if (not Has_From_File or else Move.From.File = From_File) and then
                           (not Has_From_Rank or else Move.From.Rank = From_Rank) then
                           return Move;
                        end if;
                     end if;
                  end;
               end if;
            end loop;
            
            raise Notation_Error with "No legal move matches: " & Input;
         end;
      end;
          end;
   end Parse_Move;

   function Move_To_Algebraic (State : Game_State;
                              Move  : Move_Type) return String is
      Moving_Piece : constant Board_Square := Get_Piece (State, Move.From);
      Is_Capture : constant Boolean := 
        Get_Piece (State, Move.To).Content = Occupied or 
        Move.Flag = En_Passant;
      Result : String (1 .. 10) := (others => ' ');
      Idx : Positive := 1;
      
      -- Check if disambiguation is needed
      function Need_Disambiguation return Boolean is
         Same_Piece_Moves : Move_List;
      begin
         for Legal_Move of Generate_All_Legal_Moves (State, State.Current_Player) loop
            if Legal_Move.To = Move.To and Legal_Move.From /= Move.From then
               declare
                  Other_Piece : constant Board_Square := 
                    Get_Piece (State, Legal_Move.From);
               begin
                  if Other_Piece.Content = Occupied and then
                     Other_Piece.Piece.Kind = Moving_Piece.Piece.Kind then
                     Same_Piece_Moves.Append (Legal_Move);
                  end if;
               end;
            end if;
         end loop;
         return not Same_Piece_Moves.Is_Empty;
      end Need_Disambiguation;
      
      function File_Disambiguation_Enough return Boolean is
      begin
         for Legal_Move of Generate_All_Legal_Moves (State, State.Current_Player) loop
            if Legal_Move.To = Move.To and Legal_Move.From /= Move.From and
               Legal_Move.From.File = Move.From.File then
               declare
                  Other_Piece : constant Board_Square := 
                    Get_Piece (State, Legal_Move.From);
               begin
                  if Other_Piece.Content = Occupied and then
                     Other_Piece.Piece.Kind = Moving_Piece.Piece.Kind then
                     return False;
                  end if;
               end;
            end if;
         end loop;
         return True;
      end File_Disambiguation_Enough;
   begin
      if Moving_Piece.Content = Empty then
         return "???";
      end if;
      
      -- Castling
      if Move.Flag = Castle_Kingside then
         return "O-O";
      elsif Move.Flag = Castle_Queenside then
         return "O-O-O";
      end if;
      
      -- Piece indicator (except pawns)
      if Moving_Piece.Piece.Kind /= Pawn then
         Result (Idx) := Piece_Kind_To_Char (Moving_Piece.Piece.Kind);
         Idx := Idx + 1;
         
         -- Disambiguation
         if Need_Disambiguation then
            if File_Disambiguation_Enough then
               Result (Idx) := File_To_Char (Move.From.File);
               Idx := Idx + 1;
            else
               Result (Idx) := File_To_Char (Move.From.File);
               Idx := Idx + 1;
               Result (Idx) := Character'Val (Character'Pos ('0') + Move.From.Rank);
               Idx := Idx + 1;
            end if;
         end if;
      else
         -- Pawn captures show file
         if Is_Capture then
            Result (Idx) := File_To_Char (Move.From.File);
            Idx := Idx + 1;
         end if;
      end if;
      
      -- Capture indicator
      if Is_Capture then
         Result (Idx) := 'x';
         Idx := Idx + 1;
      end if;
      
      -- Destination square
      Result (Idx) := File_To_Char (Move.To.File);
      Idx := Idx + 1;
      Result (Idx) := Character'Val (Character'Pos ('0') + Move.To.Rank);
      Idx := Idx + 1;
      
      -- Promotion
      if Move.Flag = Promotion then
         Result (Idx) := '=';
         Idx := Idx + 1;
         Result (Idx) := Piece_Kind_To_Char (Move.Promotion);
         Idx := Idx + 1;
      end if;
      
      -- Check/Checkmate
      declare
         New_State : constant Game_State := Apply_Move (State, Move);
         Opponent : constant Color_Type := Opposite_Color (State.Current_Player);
      begin
         if Is_Checkmate (New_State, Opponent) then
            Result (Idx) := '#';
            Idx := Idx + 1;
         elsif Is_In_Check (New_State, Opponent) then
            Result (Idx) := '+';
            Idx := Idx + 1;
         end if;
      end;
      
      return Trim (Result (1 .. Idx - 1), Ada.Strings.Both);
   end Move_To_Algebraic;

end Chess.Notation;
