unit uParserLog;

interface

uses
  SysUtils, Classes;

type
  PCoordinate = ^TCoordinate;
  TCoordinate = record
    Xm, Ym, Zm, Xp, Yp, Zp: Real;
    T: Integer;
  end;

  TOption = record
    Name, Value: String;
  end;

  TParserLog = class     // Парсер - лох!
  private                // Да в нем намек.
    FOptions: TStrings;  // Добрым молодцам -
    FCoordinates: TList; // ХОРЕК!
    function GetOptionCount: Integer;
    function GetCoordinateCount: Integer;   
    function GetCoordinate(Index: Integer): TCoordinate;
    function GetOption(Name: String): String;
    procedure SetCoordinate(Index: Integer; const Value: TCoordinate);
    procedure SetOption(Name: String; const Value: String);
    procedure AddCoordinate(Xm, Ym, Zm, Xp, Yp, Zp: Real; T: Integer);
    procedure AddOption(Name, Value: String);
  public
    constructor Create(FileName: String);
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(FileName: String);
    function OptionByIndex(Index: Integer): TOption;
    property Options[Name: String]: String read GetOption write SetOption;
    property OptionCount: Integer read GetOptionCount;
    property CoordinateCount: Integer read GetCoordinateCount;
    property Coordinates[Index: Integer]: TCoordinate read GetCoordinate
      write SetCoordinate;
  end;

implementation

{ ---------------------------------- TParser --------------------------------- }

procedure TParserLog.AddCoordinate(Xm, Ym, Zm, Xp, Yp, Zp: Real; T: Integer);
var Coord: PCoordinate;
begin
  New(Coord);
  Coord.Xm := Xm;
  Coord.Ym := Ym;
  Coord.Zm := Zm;
  Coord.Xp := Xp;
  Coord.Yp := Yp;
  Coord.Zp := Zp;
  Coord.T := T;
  FCoordinates.Add(Coord);
end;

procedure TParserLog.AddOption(Name, Value: String);
begin
  FOptions.Add(Name + '=' + Value);
end;

procedure TParserLog.Clear;
begin
  FOptions.Clear;
  FCoordinates.Clear;
end;

constructor TParserLog.Create(FileName: String);
begin
  FOptions := TStringList.Create;
  FCoordinates := TList.Create;
  LoadFromFile(FileName);
end;

destructor TParserLog.Destroy;
begin
  FOptions.Free;
  FCoordinates.Free;
  inherited;
end;

function TParserLog.GetCoordinate(Index: Integer): TCoordinate;
begin
  if (Index >= 0) and (Index < FCoordinates.Count) then
    Result := TCoordinate(FCoordinates[Index]^);
end;

function TParserLog.GetCoordinateCount: Integer;
begin
  Result := FCoordinates.Count; 
end;

function TParserLog.GetOption(Name: String): String;
begin
  Result := ''; 
  if FOptions.IndexOfName(Name) <> -1 then
    Result := FOptions.Values[Name];
end;

function TParserLog.GetOptionCount: Integer;
begin
  Result := FOptions.Count;
end;

procedure TParserLog.LoadFromFile(FileName: String);
var S, tmp: String;
  Name, Value: String;
  Xm, Ym, Zm, Xp, Yp, Zp: Real;
  T: Integer;
begin
  Clear;
  with TStringList.Create do
    begin
      LoadFromFile(FileName);
      S := Trim(Text);
      Free;
    end;
    
  while Pos('>', S) <> 0 do
    begin
      tmp := Copy(S, Pos('<', S) + 1, Pos('>', S) - 2);
      Delete(S, 1, Pos('>', S));
      
      if Pos('option Name', tmp) <> 0 then
        begin
          Delete(tmp, 1, Pos('=', tmp));
          Name := Copy(tmp, 2, Pos('" ', tmp) - 2);
          Delete(tmp, 1, Pos('=', tmp));
          Value := Copy(tmp, 2, Pos('" ', tmp) - 2);

          AddOption(Name, Value);
        end
      else
        if Pos('point', tmp) <> 0 then
          begin
            tmp := StringReplace(tmp, '.', ',', [rfReplaceAll]);
            Delete(tmp, 1, Pos('=', tmp));
            T := StrToInt(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Xp := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Zp := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Yp := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Xm := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Zm := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));
            Delete(tmp, 1, Pos('=', tmp));
            Ym := StrToFloat(Copy(tmp, 2, Pos('" ', tmp) - 2));

            AddCoordinate(Xm, Ym, Zm, Xp, Yp, Zp, T);
          end;
    end;
end;

function TParserLog.OptionByIndex(Index: Integer): TOption;
begin
  if (Index >= 0) and (Index < FOptions.Count) then
    begin
      Result.Name := FOptions.Names[Index];
      Result.Value := FOptions.ValueFromIndex[Index];
    end;
end;

procedure TParserLog.SetCoordinate(Index: Integer; const Value: TCoordinate);
begin
  if (Index >= 0) and (Index < FCoordinates.Count) then
    begin
      PCoordinate(FCoordinates[Index]).Xm := Value.Xm;
      PCoordinate(FCoordinates[Index]).Ym := Value.Ym;
      PCoordinate(FCoordinates[Index]).Zm := Value.Zm;
      PCoordinate(FCoordinates[Index]).Xp := Value.Xp;
      PCoordinate(FCoordinates[Index]).Yp := Value.Yp;
      PCoordinate(FCoordinates[Index]).Zp := Value.Zp;
      PCoordinate(FCoordinates[Index]).T := Value.T;
    end;
end;

procedure TParserLog.SetOption(Name: String; const Value: String);
begin
  if FOptions.IndexOfName(Name) <> -1 then
    FOptions.Values[Name] := Value;
end;

end.
