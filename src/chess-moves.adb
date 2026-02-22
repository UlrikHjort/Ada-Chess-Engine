-- ***************************************************************************
--                      Chess - Move Generation Body
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

with Chess.Board;    use Chess.Board;
with Chess.Bitboard; use Chess.Bitboard;

package body Chess.Moves is

   type Offset_Type is record
      File_Off : Integer;
      Rank_Off : Integer;
   end record;

   type Offset_Array is array (Natural range <>) of Offset_Type;

   function File_Offset (F : File_Type; Offset : Integer) return File_Type is
   begin
      return File_Type'Val (File_Type'Pos (F) + Offset);
   end File_Offset;

   function Rank_Offset (R : Rank_Type; Offset : Integer) return Rank_Type is
   begin
      return Rank_Type (Integer (R) + Offset);
   end Rank_Offset;

   function Is_Valid_File_Offset (F : File_Type; Offset : Integer) return Boolean is
      Pos : constant Integer := File_Type'Pos (F) + Offset;
   begin
      return Pos >= File_Type'Pos (File_Type'First) and 
             Pos <= File_Type'Pos (File_Type'Last);
   end Is_Valid_File_Offset;

   function Is_Valid_Rank_Offset (R : Rank_Type; Offset : Integer) return Boolean is
      Val : constant Integer := Integer (R) + Offset;
   begin
      return Val >= Integer (Rank_Type'First) and 
             Val <= Integer (Rank_Type'Last);
   end Is_Valid_Rank_Offset;

   procedure Add_Move_If_Valid (Moves  : in out Move_List;
                                State  : Game_State;
                                From   : Square_Type;
                                To_File : File_Type;
                                To_Rank : Rank_Type;
                                Our_Color : Color_Type) is
      To_Square : constant Square_Type := (To_File, To_Rank);
      Target : constant Board_Square := Get_Piece (State, To_Square);
   begin
      if Target.Content = Empty then
         Moves.Append ((From, To_Square, Normal, Queen));
      elsif Target.Piece.Color /= Our_Color then
         Moves.Append ((From, To_Square, Normal, Queen));
      end if;
   end Add_Move_If_Valid;

   function Generate_Pawn_Moves (State  : Game_State;
                                Square : Square_Type;
                                Color  : Color_Type) return Move_List is
      Moves : Move_List;
      Direction : constant Integer := (if Color = White then 1 else -1);
      Start_Rank : constant Rank_Type := (if Color = White then 2 else 7);
      Promotion_Rank : constant Rank_Type := (if Color = White then 8 else 1);
      
      Forward_Rank : Rank_Type;
      Double_Rank : Rank_Type;
   begin
      -- Single push
      if Is_Valid_Rank_Offset (Square.Rank, Direction) then
         Forward_Rank := Rank_Offset (Square.Rank, Direction);
         if Get_Piece (State, (Square.File, Forward_Rank)).Content = Empty then
            if Forward_Rank = Promotion_Rank then
               -- Promotions
               for Promo in Knight .. Queen loop
                  Moves.Append ((Square, (Square.File, Forward_Rank), 
                                Promotion, Promo));
               end loop;
            else
               Moves.Append ((Square, (Square.File, Forward_Rank), Normal, Queen));
            end if;
            
            -- Double push from start position
            if Square.Rank = Start_Rank and 
               Is_Valid_Rank_Offset (Square.Rank, Direction * 2) then
               Double_Rank := Rank_Offset (Square.Rank, Direction * 2);
               if Get_Piece (State, (Square.File, Double_Rank)).Content = Empty then
                  Moves.Append ((Square, (Square.File, Double_Rank), Normal, Queen));
               end if;
            end if;
         end if;
      end if;
      
      -- Captures
      for File_Dir in -1 .. 1 loop
         if File_Dir /= 0 and then
            Is_Valid_File_Offset (Square.File, File_Dir) and then
            Is_Valid_Rank_Offset (Square.Rank, Direction) then
            
            declare
               Target_File : constant File_Type := File_Offset (Square.File, File_Dir);
               Target_Rank : constant Rank_Type := Rank_Offset (Square.Rank, Direction);
               Target : constant Board_Square := 
                 Get_Piece (State, (Target_File, Target_Rank));
            begin
               if Target.Content = Occupied and then 
                  Target.Piece.Color /= Color then
                  if Target_Rank = Promotion_Rank then
                     for Promo in Knight .. Queen loop
                        Moves.Append ((Square, (Target_File, Target_Rank),
                                      Promotion, Promo));
                     end loop;
                  else
                     Moves.Append ((Square, (Target_File, Target_Rank), Normal, Queen));
                  end if;
               end if;
               
               -- En passant
               if State.Has_En_Passant and then
                  State.En_Passant.File = Target_File and then
                  State.En_Passant.Rank = Target_Rank then
                  Moves.Append ((Square, (Target_File, Target_Rank), 
                               En_Passant, Queen));
               end if;
            end;
         end if;
      end loop;
      
      return Moves;
   end Generate_Pawn_Moves;

   function Generate_Knight_Moves (State  : Game_State;
                                   Square : Square_Type;
                                   Color  : Color_Type) return Move_List is
      Moves : Move_List;
      Knight_Offsets : constant Offset_Array := (
         (-2, -1), (-2, 1), (-1, -2), (-1, 2),
         (1, -2), (1, 2), (2, -1), (2, 1)
      );
   begin
      for Offset of Knight_Offsets loop
         if Is_Valid_File_Offset (Square.File, Offset.File_Off) and then
            Is_Valid_Rank_Offset (Square.Rank, Offset.Rank_Off) then
            Add_Move_If_Valid (Moves, State, Square,
                             File_Offset (Square.File, Offset.File_Off),
                             Rank_Offset (Square.Rank, Offset.Rank_Off),
                             Color);
         end if;
      end loop;
      return Moves;
   end Generate_Knight_Moves;

   function Generate_Sliding_Moves (State       : Game_State;
                                    Square      : Square_Type;
                                    Color       : Color_Type;
                                    Directions  : Offset_Array) return Move_List is
      Moves : Move_List;
   begin
      for Dir of Directions loop
         for Distance in 1 .. 7 loop
            declare
               File_Off : constant Integer := Dir.File_Off * Distance;
               Rank_Off : constant Integer := Dir.Rank_Off * Distance;
            begin
               if not Is_Valid_File_Offset (Square.File, File_Off) or else
                  not Is_Valid_Rank_Offset (Square.Rank, Rank_Off) then
                  exit;
               end if;
               
               declare
                  To_File : constant File_Type := File_Offset (Square.File, File_Off);
                  To_Rank : constant Rank_Type := Rank_Offset (Square.Rank, Rank_Off);
                  Target : constant Board_Square := Get_Piece (State, (To_File, To_Rank));
               begin
                  if Target.Content = Empty then
                     Moves.Append ((Square, (To_File, To_Rank), Normal, Queen));
                  elsif Target.Piece.Color /= Color then
                     Moves.Append ((Square, (To_File, To_Rank), Normal, Queen));
                     exit;  -- Can't move past captured piece
                  else
                     exit;  -- Blocked by our own piece
                  end if;
               end;
            end;
         end loop;
      end loop;
      return Moves;
   end Generate_Sliding_Moves;

   function Generate_Bishop_Moves (State  : Game_State;
                                   Square : Square_Type;
                                   Color  : Color_Type) return Move_List is
      Diagonal_Directions : constant Offset_Array := (
         (-1, -1), (-1, 1), (1, -1), (1, 1)
      );
   begin
      return Generate_Sliding_Moves (State, Square, Color, Diagonal_Directions);
   end Generate_Bishop_Moves;

   function Generate_Rook_Moves (State  : Game_State;
                                 Square : Square_Type;
                                 Color  : Color_Type) return Move_List is
      Orthogonal_Directions : constant Offset_Array := (
         (-1, 0), (1, 0), (0, -1), (0, 1)
      );
   begin
      return Generate_Sliding_Moves (State, Square, Color, Orthogonal_Directions);
   end Generate_Rook_Moves;

   function Generate_Queen_Moves (State  : Game_State;
                                  Square : Square_Type;
                                  Color  : Color_Type) return Move_List is
      All_Directions : constant Offset_Array := (
         (-1, -1), (-1, 0), (-1, 1), (0, -1),
         (0, 1), (1, -1), (1, 0), (1, 1)
      );
   begin
      return Generate_Sliding_Moves (State, Square, Color, All_Directions);
   end Generate_Queen_Moves;

   function Generate_King_Moves (State  : Game_State;
                                 Square : Square_Type;
                                 Color  : Color_Type) return Move_List is
      Moves : Move_List;
      King_Offsets : constant Offset_Array := (
         (-1, -1), (-1, 0), (-1, 1), (0, -1),
         (0, 1), (1, -1), (1, 0), (1, 1)
      );
      Rank : constant Rank_Type := (if Color = White then 1 else 8);
   begin
      -- Normal king moves
      for Offset of King_Offsets loop
         if Is_Valid_File_Offset (Square.File, Offset.File_Off) and then
            Is_Valid_Rank_Offset (Square.Rank, Offset.Rank_Off) then
            Add_Move_If_Valid (Moves, State, Square,
                             File_Offset (Square.File, Offset.File_Off),
                             Rank_Offset (Square.Rank, Offset.Rank_Off),
                             Color);
         end if;
      end loop;
      
      -- Castling
      if Square.Rank = Rank and Square.File = File_E then
         -- Kingside castling
         if (Color = White and State.Castling.White_Kingside) or
            (Color = Black and State.Castling.Black_Kingside) then
            if Get_Piece (State, (File_F, Rank)).Content = Empty and
               Get_Piece (State, (File_G, Rank)).Content = Empty then
               Moves.Append ((Square, (File_G, Rank), Castle_Kingside, Queen));
            end if;
         end if;
         
         -- Queenside castling
         if (Color = White and State.Castling.White_Queenside) or
            (Color = Black and State.Castling.Black_Queenside) then
            if Get_Piece (State, (File_D, Rank)).Content = Empty and
               Get_Piece (State, (File_C, Rank)).Content = Empty and
               Get_Piece (State, (File_B, Rank)).Content = Empty then
               Moves.Append ((Square, (File_C, Rank), Castle_Queenside, Queen));
            end if;
         end if;
      end if;
      
      return Moves;
   end Generate_King_Moves;

   function Generate_Pseudo_Legal_Moves (State  : Game_State;
                                         Square : Square_Type) 
                                        return Move_List is
      Piece : constant Board_Square := Get_Piece (State, Square);
   begin
      if Piece.Content = Empty then
         return Move_Vectors.Empty_Vector;
      end if;
      
      case Piece.Piece.Kind is
         when Pawn =>
            return Generate_Pawn_Moves (State, Square, Piece.Piece.Color);
         when Knight =>
            return Generate_Knight_Moves (State, Square, Piece.Piece.Color);
         when Bishop =>
            return Generate_Bishop_Moves (State, Square, Piece.Piece.Color);
         when Rook =>
            return Generate_Rook_Moves (State, Square, Piece.Piece.Color);
         when Queen =>
            return Generate_Queen_Moves (State, Square, Piece.Piece.Color);
         when King =>
            return Generate_King_Moves (State, Square, Piece.Piece.Color);
      end case;
   end Generate_Pseudo_Legal_Moves;

   function Generate_All_Pseudo_Legal_Moves (State : Game_State;
                                             Color : Color_Type) 
                                            return Move_List is
      All_Moves : Move_List;
   begin
      for F in File_Type loop
         for R in Rank_Type loop
            declare
               Square : constant Square_Type := (F, R);
               Piece : constant Board_Square := Get_Piece (State, Square);
            begin
               if Piece.Content = Occupied and then Piece.Piece.Color = Color then
                  All_Moves.Append (Generate_Pseudo_Legal_Moves (State, Square));
               end if;
            end;
         end loop;
      end loop;
      return All_Moves;
   end Generate_All_Pseudo_Legal_Moves;

   function Find_King (State : Game_State;
                      Color : Color_Type) return Square_Type is
   begin
      for F in File_Type loop
         for R in Rank_Type loop
            declare
               Square : constant Square_Type := (F, R);
               Piece : constant Board_Square := Get_Piece (State, Square);
            begin
               if Piece.Content = Occupied and then
                  Piece.Piece.Kind = King and then
                  Piece.Piece.Color = Color then
                  return Square;
               end if;
            end;
         end loop;
      end loop;
      raise Program_Error with "King not found on board";
   end Find_King;

   function Is_Square_Attacked (State    : Game_State;
                               Square   : Square_Type;
                               By_Color : Color_Type) return Boolean is
   begin
      return Is_Square_Attacked_BB (State, Square, By_Color);
   end Is_Square_Attacked;

   function Is_In_Check (State : Game_State;
                        Color : Color_Type) return Boolean is
      King_Square : constant Square_Type := Find_King (State, Color);
   begin
      return Is_Square_Attacked (State, King_Square, Opposite_Color (Color));
   end Is_In_Check;

   function Apply_Move (State : Game_State;
                       Move  : Move_Type) return Game_State is
      New_State : Game_State := State;
      Moving_Piece : constant Board_Square := Get_Piece (State, Move.From);
   begin
      -- Move the piece
      New_State.Board (Move.To.File, Move.To.Rank) := Moving_Piece;
      New_State.Board (Move.From.File, Move.From.Rank) := (Content => Empty);
      
      -- Handle special moves
      case Move.Flag is
         when Castle_Kingside =>
            declare
               Rank : constant Rank_Type := Move.From.Rank;
               Rook : constant Board_Square := Get_Piece (State, (File_H, Rank));
            begin
               New_State.Board (File_F, Rank) := Rook;
               New_State.Board (File_H, Rank) := (Content => Empty);
            end;
            
         when Castle_Queenside =>
            declare
               Rank : constant Rank_Type := Move.From.Rank;
               Rook : constant Board_Square := Get_Piece (State, (File_A, Rank));
            begin
               New_State.Board (File_D, Rank) := Rook;
               New_State.Board (File_A, Rank) := (Content => Empty);
            end;
            
         when En_Passant =>
            declare
               Captured_Rank : constant Rank_Type := Move.From.Rank;
            begin
               New_State.Board (Move.To.File, Captured_Rank) := (Content => Empty);
            end;
            
         when Promotion =>
            New_State.Board (Move.To.File, Move.To.Rank) := 
              (Content => Occupied, 
               Piece => (Kind => Move.Promotion, 
                        Color => Moving_Piece.Piece.Color));
               
         when Normal =>
            null;
      end case;
      
      -- Update castling rights
      if Moving_Piece.Piece.Kind = King then
         if Moving_Piece.Piece.Color = White then
            New_State.Castling.White_Kingside := False;
            New_State.Castling.White_Queenside := False;
         else
            New_State.Castling.Black_Kingside := False;
            New_State.Castling.Black_Queenside := False;
         end if;
      elsif Moving_Piece.Piece.Kind = Rook then
         if Move.From.File = File_A and Move.From.Rank = 1 then
            New_State.Castling.White_Queenside := False;
         elsif Move.From.File = File_H and Move.From.Rank = 1 then
            New_State.Castling.White_Kingside := False;
         elsif Move.From.File = File_A and Move.From.Rank = 8 then
            New_State.Castling.Black_Queenside := False;
         elsif Move.From.File = File_H and Move.From.Rank = 8 then
            New_State.Castling.Black_Kingside := False;
         end if;
      end if;
      
      -- Update en passant
      New_State.Has_En_Passant := False;
      if Moving_Piece.Piece.Kind = Pawn then
         if abs (Integer (Move.To.Rank) - Integer (Move.From.Rank)) = 2 then
            New_State.Has_En_Passant := True;
            New_State.En_Passant := 
              (Move.From.File, 
               Rank_Type ((Integer (Move.From.Rank) + Integer (Move.To.Rank)) / 2));
         end if;
      end if;
      
      -- Update halfmove clock
      if Moving_Piece.Piece.Kind = Pawn or 
         Get_Piece (State, Move.To).Content = Occupied then
         New_State.Halfmove_Clock := 0;
      else
         New_State.Halfmove_Clock := New_State.Halfmove_Clock + 1;
      end if;
      
      -- Update move counters
      if State.Current_Player = Black then
         New_State.Fullmove_Number := New_State.Fullmove_Number + 1;
      end if;
      New_State.Current_Player := Opposite_Color (State.Current_Player);
      
      return New_State;
   end Apply_Move;

   function Is_Legal_Move (State : Game_State;
                          Move  : Move_Type) return Boolean is
      Test_State : constant Game_State := Apply_Move (State, Move);
      Moving_Piece : constant Board_Square := Get_Piece (State, Move.From);
   begin
      if Moving_Piece.Content = Empty then
         return False;
      end if;
      
      -- Check if the move leaves our king in check
      if Is_In_Check (Test_State, Moving_Piece.Piece.Color) then
         return False;
      end if;
      
      -- Additional validation for castling
      if Move.Flag = Castle_Kingside or Move.Flag = Castle_Queenside then
         -- Can't castle out of check
         if Is_In_Check (State, Moving_Piece.Piece.Color) then
            return False;
         end if;
         
         -- Can't castle through check
         declare
            Through_Square : Square_Type;
         begin
            if Move.Flag = Castle_Kingside then
               Through_Square := (File_F, Move.From.Rank);
            else
               Through_Square := (File_D, Move.From.Rank);
            end if;
            
            if Is_Square_Attacked (State, Through_Square, 
                                  Opposite_Color (Moving_Piece.Piece.Color)) then
               return False;
            end if;
         end;
      end if;
      
      return True;
   end Is_Legal_Move;

   function Generate_Legal_Moves (State  : Game_State;
                                 Square : Square_Type) 
                                return Move_List is
      Pseudo_Legal : constant Move_List := Generate_Pseudo_Legal_Moves (State, Square);
      Legal : Move_List;
   begin
      for Move of Pseudo_Legal loop
         if Is_Legal_Move (State, Move) then
            Legal.Append (Move);
         end if;
      end loop;
      return Legal;
   end Generate_Legal_Moves;

   function Generate_All_Legal_Moves (State : Game_State;
                                     Color : Color_Type) 
                                    return Move_List is
      Pseudo_Legal : constant Move_List := Generate_All_Pseudo_Legal_Moves (State, Color);
      Legal : Move_List;
   begin
      for Move of Pseudo_Legal loop
         if Is_Legal_Move (State, Move) then
            Legal.Append (Move);
         end if;
      end loop;
      return Legal;
   end Generate_All_Legal_Moves;

   function Is_Checkmate (State : Game_State;
                         Color : Color_Type) return Boolean is
   begin
      return Is_In_Check (State, Color) and then
             Generate_All_Legal_Moves (State, Color).Is_Empty;
   end Is_Checkmate;

   function Is_Stalemate (State : Game_State;
                         Color : Color_Type) return Boolean is
   begin
      return not Is_In_Check (State, Color) and then
             Generate_All_Legal_Moves (State, Color).Is_Empty;
   end Is_Stalemate;

end Chess.Moves;
