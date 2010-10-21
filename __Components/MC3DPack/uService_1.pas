unit uService_1;

interface

uses Classes, SysUtils, Windows, Forms, Buttons;

type
  TStrArr = array of string;

  TErrorType = (etResTablesNotFound);

  TArrowDirection = (adTop, adBottom, adLeft, adRight);

  TChars = set of Char;

function MakeErrorMessage(ErrorType: TErrorType; ModuleName, OutMessage: String): String;
function SplitString(Splitter, Str: string): TStrArr;
function GetCompName: String;
procedure ChangeArrows(Visible: boolean; Sender: TObject; Direction: TArrowDirection);
procedure MemoryNormal(Handle: HWND);
function StrToHexCharCodes(S: String): String;
function HexCharCodesToStr(S: String): String;
function HexToChar(S: String): String;
function LeaveChars(Chars: TChars; S: String): String;
function GetFileSize(FName: String): Int64;

implementation

function MakeErrorMessage(ErrorType: TErrorType; ModuleName,
  OutMessage: String): String;
begin
  case ErrorType of
    etResTablesNotFound:
      Result := 'Не удалось восстановить неоходимые для модуля ' + ModuleName +
        ' таблицы. ' + OutMessage;
  end;
end;

function SplitString(Splitter, Str: string): TStrArr;
begin
  SetLength(Result, 0);
  if Copy(Str, Length(Str) - Length(Splitter) + 1, Length(Splitter)) <> Splitter then
    Str := Str + Splitter;
  while pos(Splitter, Str) <> 0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Trim(Copy(Str, 1, pos(Splitter, Str) - 1));
      Delete(Str, 1, Pos(Splitter, Str) + Length(Splitter) - 1);
    end;
end;

function GetCompName: String;
var CompName: array[0..MAX_COMPUTERNAME_LENGTH + 1] of char;
  CompNameSize: DWORD;
begin
  CompNameSize := MAX_COMPUTERNAME_LENGTH + 1;
  GetComputerName(@CompName, CompNameSize);
  Result := String(CompName);
end;

////////////////////////////////////////////////////////////////////////////////
// СТРЕЛОЧКИ НА КНОПОЧКАХ
procedure ChangeArrows(Visible: boolean; Sender: TObject;
  Direction: TArrowDirection);
var S: String;
begin
  case Direction of
    adTop:
      if Visible then
        S := 'PANEL_HIDE_BTN'
      else S := 'PANEL_SHOW_BTN';
    adBottom:
      if Visible then
        S := 'PANEL_SHOW_BTN'
      else S := 'PANEL_HIDE_BTN';
    adLeft:
      if Visible then
        S := 'PANEL_HIDE_VERT_BTN'
      else S := 'PANEL_SHOW_VERT_BTN';
    adRight:
      if Visible then
        S := 'PANEL_SHOW_VERT_BTN'
      else S := 'PANEL_HIDE_VERT_BTN';
  end;
  if FindResource(hInstance, PAnsiChar(s), RT_BITMAP) <> 0 then
    TSpeedButton(Sender).Glyph.LoadFromResourceName(hInstance, S);
end;

////////////////////////////////////////////////////////////////////////////////
// ОЧИСТКА ПАМЯТИ
procedure MemoryNormal(Handle: HWND);
var Animation: Boolean;
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Animation := Info.iMinAnimate <> 0
  else Animation := False;
  if Animation then BOOL(Info.iMinAnimate) := False;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
  ShowWindow(Handle, SW_MINIMIZE);
  ShowWindow(Handle, SW_NORMAL);
  if Animation then BOOL(Info.iMinAnimate) := True;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

// Функция, которая преобразовывает строку в строку хексарных кодов символов
function StrToHexCharCodes(S: String): String;
var i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + IntToHex(Ord(S[i]), 2);
end;

// Обратная хуета
function HexCharCodesToStr(S: String): String;
var i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(S) do
    begin
      Result := Result + HexToChar(S[i] + S[i + 1]);
      Inc(i, 2);
    end;
end;

function HexToChar(S: String): String;
var
  i, Res: Integer;
begin
  Result := '';
  S := AnsiUpperCase(S);
  Res := 0;
  for i := 1 to Length(S) do
    begin
      case S[i] of
        '0'..'9':
          if (i mod 2 = 1) then
            Res := StrToInt(S[i])
          else Res := Res * 16 + StrToInt(S[i]);
        'A'..'F':
          if (i mod 2 = 1) then
            Res := Ord(S[i]) - 55
          else Res := Res * 16 + Ord(S[i]) - 55;
      end;
      if (i mod 2 = 0) then Result := Result + Char(Res);
    end;
end;

function LeaveChars(Chars: TChars; S: String): String;
var i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] in Chars then Result := Result + S[i];
end;

function GetFileSize(FName: String): Int64;
var SR: TSearchRec;
begin
  if SysUtils.FindFirst(FName, faAnyFile, SR) = 0 then
    begin
      Result := SR.Size;
      SysUtils.FindClose(SR);
    end
  else Result := -1;
end;

end.
