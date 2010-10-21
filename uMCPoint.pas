unit uMCPoint;

interface

uses
  SysUtils, Classes, LibXmlParser, uParams, Windows;

type
  T3DPoint = record
    X, Y, Z: Real;
  end;

  T3DPointList = class(TList)
  private
    FTime: Cardinal;
    function GetItems(Index: Integer): T3DPoint;
    procedure SetItems(Index: Integer; const Value: T3DPoint);
  public
    constructor Create(const ATime: Cardinal);
    function Add(const APoint: T3DPoint): Integer; reintroduce;
    procedure Clear; override;
    property Items[Index: Integer]: T3DPoint read GetItems write SetItems; default;
    property Time: Cardinal read FTime write FTime;
  end;


type
  TMCFile = class;

  // MC - Motion Check
  TMCPoint = class
  private
    FOwner: TMCFile;
    FPoints: T3DPointList;
    FIndex: Integer;
    function GetMetricPoint(const Index: Integer): T3DPoint;
    function GetTime: Cardinal;
    procedure SetTime(const Value: Cardinal);
    function GetCount: Integer;
    function GetPoint(const Index: Integer): T3DPoint;
  protected
    procedure OrderPoints;
  public
    constructor Create(AOwner: TMCFile); overload;
    constructor Create(AOwner: TMCFile; const ATime: Cardinal); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const X, Y, Z: Real);
    procedure AddList(const List: T3DPointList);
    procedure SetList(const List: T3DPointList);
    property Points[const Index: Integer]: T3DPoint read GetPoint;
    property Time: Cardinal read GetTime write SetTime;
    property Count: Integer read GetCount;
    property PointsMetric[const Index: Integer]: T3DPoint read GetMetricPoint;
  end;

  TMCNodeType = (ntNone, ntRoot, ntOptions, ntOption, ntCoordinateRoot, ntPoints, ntPoint);

  TMCFile = class
  private
    FXmlParser: TXmlParser;
    FOptions: TParams;
    FCoordinates: TList;
    function GetOption(const Name: String): TParam;
    function GetCoordinateCount: Integer;
    function GetCoordinate(const Index: Integer): TMCPoint;
    function CheckNodeType(const Name: String): TMCNodeType;
  protected
    procedure OrderPoints;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFile(const FileName: String);
    procedure SaveFile(const FileName: String);
    function AddCoordinate: TMCPoint;
    property Coordinates[const Index: Integer]: TMCPoint read GetCoordinate;
    property CoordinateCount: Integer read GetCoordinateCount;
    property Options[const Name: String]: TParam read GetOption;
  end;

implementation

uses
  Math;

{ T3DPointList }

function T3DPointList.Add(const APoint: T3DPoint): Integer;
var
  P: Pointer;
begin
  GetMem(P, SizeOf(T3DPoint));
  T3DPoint(P^) := APoint;
  Result := inherited Add(P);
end;

procedure T3DPointList.Clear;
var
  I: integer;
begin
  FTime := 0;
  for I := Count - 1 downto 0 do
    Delete(I);
  inherited Clear;
end;

constructor T3DPointList.Create(const ATime: Cardinal);
begin
  inherited Create;
  FTime := ATime;
end;

function T3DPointList.GetItems(Index: Integer): T3DPoint;
begin
  Result := T3DPoint((inherited Items[Index])^);
end;

procedure T3DPointList.SetItems(Index: Integer; const Value: T3DPoint);
begin
  inherited Items[Index] := @Value;
end;

{ TMCPoint }

procedure TMCPoint.Add(const X, Y, Z: Real);
var
  P: T3DPoint;
begin
  P.X := X;
  P.Y := Y;
  P.Z := Z;
  FPoints.Add(P);
end;

procedure TMCPoint.AddList(const List: T3DPointList);
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
    FPoints.Add(List[I]);
end;

procedure TMCPoint.Clear;
begin
  FPoints.Clear;
end;

constructor TMCPoint.Create(AOwner: TMCFile);
begin
  FOwner := AOwner;
  FIndex := -1;
end;

constructor TMCPoint.Create(AOwner: TMCFile; const ATime: Cardinal);
begin
  FPoints := T3DPointList.Create(ATime);
  FOwner := AOwner;
  FIndex := -1;
end;

destructor TMCPoint.Destroy;
begin
  FreeAndNil(FPoints);
  inherited;
end;

function TMCPoint.GetCount: Integer;
begin
  Result := FPoints.Count;
end;

function TMCPoint.GetMetricPoint(const Index: Integer): T3DPoint;
var
  Xpx1, Xpx2, Ypx1,
  R,
  TanAw1, TanAh1, TanAw2,
  Xm, Ym, Zm: Real;
  Wpx1, Hpx1, Wpx2: Integer;
begin
  Wpx1   := FOwner.Options['Cam1ResX'].AsInteger;
  Hpx1   := FOwner.Options['Cam1ResY'].AsInteger;
  Wpx2   := FOwner.Options['Cam2ResX'].AsInteger;

  if (FPoints[Index].X = -1) then
    Xpx1 := 0
  else
    Xpx1 := FPoints[Index].X - Wpx1 / 2;
  if (FPoints[Index].Y = -1) then
    Xpx2 := 0
  else
    Xpx2 := FPoints[Index].Y - Wpx2 / 2;
  Ypx1   := FPoints[Index].Z; // - Wpx2 / 2;

  // надо еще учеть что Xpx1=(Xpx1 - Cam1ResX / 2)
  //Xpx1   := (Xpx1 - Wpx1) / 2;

  R      := FOwner.Options['CameraRadius'].AsFloat;

  TanAw1 := 5/3 * Tan(FOwner.Options['Cam1Degree'].AsFloat * Pi / 180);
  TanAh1 := 5/4 * Tan(FOwner.Options['Cam1Degree'].AsFloat * Pi / 180);
  TanAw2 := 5/3 * Tan(FOwner.Options['Cam2Degree'].AsFloat * Pi / 180);

  Xm     := (2 * R * Xpx1 * TanAw1 * (Wpx2 + 2 * Xpx2 * TanAw2)) / (Wpx1 * Wpx2 - 4 * Xpx1 * Xpx2 * TanAw1 * TanAw2);
  Ym     := (2 * R * Xpx2 * TanAw2 * (Wpx1 + 2 * Xpx1 * TanAw1)) / (Wpx1 * Wpx2 - 4 * Xpx1 * Xpx2 * TanAw1 * TanAw2);
  Zm     := Ypx1 * (R + Ym) * 2 * TanAh1 / Hpx1;

  Result.X := Xm;
  Result.Y := Zm;
  Result.Z := Ym;
end;

function TMCPoint.GetPoint(const Index: Integer): T3DPoint;
begin
  if InRange(Index, 0, Count - 1) then
    Result := FPoints[Index];
end;

function TMCPoint.GetTime: Cardinal;
begin
  Result := FPoints.Time;
end;

procedure TMCPoint.OrderPoints;

  // Разность пройденых расстояний для двух ближайших точек
  function GetPointDiff(const p1, p2, p3: T3DPoint): Real;
  begin
    Result := Sqrt(Sqr(p1.X - p2.X) + Sqr(p1.Y - p2.Y) + Sqr(p1.Z - p2.Z)) -
      Sqrt(Sqr(p2.X - p3.X) + Sqr(p2.Y - p3.Y) + Sqr(p2.Z - p3.Z));
  end;

var
  I, J, MinDiffIndex: Integer;
  PrevPoint, PPrevPoint: T3DPointList;
  MinDiff, DiffRun: Real;
  temp: T3DPoint;
begin
  if FIndex <= 1 then
    Exit;

  PrevPoint := FOwner.Coordinates[FIndex - 1].FPoints;
  PPrevPoint := FOwner.Coordinates[FIndex - 2].FPoints;
  for I := 0 to PrevPoint.Count - 1 do
    begin
      MinDiffIndex := 0;
      MinDiff := GetPointDiff(PPrevPoint[I], PrevPoint[I], FPoints[0]);
      // Ищем минимум
      for J := 0 to FPoints.Count - 1 do
        begin
          DiffRun := GetPointDiff(PPrevPoint[I], PrevPoint[I], FPoints[J]);
          if MinDiff < DiffRun then
            begin
              MinDiff := DiffRun;
              MinDiffIndex := J;
            end;
        end;
      // Если минимум не тоже, что и в предыдущей точке, то меняем местами
      if MinDiffIndex <> I then
        begin
          temp := FPoints[MinDiffIndex];
          FPoints[MinDiffIndex] := FPoints[I];
          FPoints[I] := temp;
        end;
    end;          
end;

procedure TMCPoint.SetList(const List: T3DPointList);
var
  I: Integer;
begin
  if Assigned(FPoints) then
    FreeAndNil(FPoints);
  FPoints := T3DPointList.Create(List.Time);
  for I := 0 to List.Count - 1 do
    FPoints.Add(List[I]);
end;

procedure TMCPoint.SetTime(const Value: Cardinal);
begin
  FPoints.Time := Value;
end;

{ TMCFile }

function TMCFile.AddCoordinate: TMCPoint;
begin
  Result := TMCPoint.Create(Self);
  Result.FIndex := FCoordinates.Count;
  FCoordinates.Add(Result);
end;

function TMCFile.CheckNodeType(const Name: String): TMCNodeType;
begin
  Result := ntNone;
  if Name = 'test' then
    Result := ntRoot
  else if Name = 'options' then
    Result := ntOptions
  else if Name = 'option' then
    Result := ntOption
  else if Name = 'coordinates' then
    Result := ntCoordinateRoot
  else if Name = 'points' then
    Result := ntPoints
  else if Name = 'point' then
    Result := ntPoint;
end;

procedure TMCFile.Clear;
var
  I: Integer;
begin
  FOptions.Clear;
  for I := 0 to CoordinateCount - 1 do
    Coordinates[I].Clear;
  FCoordinates.Clear;
end;

constructor TMCFile.Create;
begin
  FXmlParser := TXmlParser.Create;
  FOptions := TParams.Create('', False);
  FOptions.SaveOnDestroy := False;
  FOptions.SaveOnWrite := False;
  FCoordinates := TList.Create;
end;

destructor TMCFile.Destroy;
begin
  FreeAndNil(FXmlParser);
  FreeAndNil(FOptions);
  FreeAndNil(FCoordinates);
  inherited;
end;

function TMCFile.GetCoordinate(const Index: Integer): TMCPoint;
begin
  Result := nil;
  if InRange(Index, 0, CoordinateCount - 1) then
    Result := TMCPoint(FCoordinates[Index]);
end;

function TMCFile.GetCoordinateCount: Integer;
begin
  Result := FCoordinates.Count;
end;

function TMCFile.GetOption(const Name: String): TParam;
begin
  Result := FOptions[Name];
end;

procedure TMCFile.LoadFile(const FileName: String);
begin
  Clear;
  with FXmlParser do
    begin
      LoadFromFile(ExpandFileName(FileName), fmOpenRead or fmShareDenyWrite);
      StartScan;
      while Scan do
        if CurPartType in [ptStartTag, ptEmptyTag] then
          case CheckNodeType(CurName) of
            ntNone: ; // Dummy
            ntRoot: ; // Dummy
            ntOptions: ; // Dummy
            ntOption:
              FOptions[CurAttr.Value('Name')].AsString := CurAttr.Value('Value');
            ntCoordinateRoot: ; // Dummy
            ntPoints:
              FCoordinates.Add(TMCPoint.Create(Self, StrToInt(CurAttr.Value('T'))));
            ntPoint:
              TMCPoint(Coordinates[CoordinateCount - 1]).Add(
                StrToFloat(CurAttr.Value('X')),
                StrToFloat(CurAttr.Value('Y')),
                StrToFloat(CurAttr.Value('Z'))
              );
          end;
    end;
end;

procedure TMCFile.OrderPoints;
var
  I: Integer;
begin
  for I := 2 to CoordinateCount - 1 do
    Coordinates[I].OrderPoints;
end;

// Serialize to XML
procedure TMCFile.SaveFile(const FileName: String);

  procedure WriteStr(const S: String; const F: Integer);
  var
    P: PChar;
  begin
    P := PChar(S);
    SysUtils.FileWrite(F, P^, StrLen(P));
  end;

var
  F, I, J: Integer;
begin
  OrderPoints;
  F := SysUtils.FileCreate(FileName);
  if F > 0 then
    begin
      WriteStr('<?xml version="1.0" encoding="windows-1251" ?>'#13#10, F);
      WriteStr('<test>'#13#10, F);
      WriteStr('  <options>'#13#10, F);
      for I := 0 to FOptions.Count - 1 do
        WriteStr('    <option Name="' + FOptions.ParamByIndex(I).Name + '" Value="' + FOptions.ParamByIndex(I).AsString + '" />'#13#10, F);
      WriteStr('  </options>'#13#10, F);
      WriteStr('  <coordinates>'#13#10, F);
      for I := 0 to CoordinateCount - 1 do
        begin
          WriteStr('    <points T="' + IntToStr(Coordinates[I].Time) + '">'#13#10, F);
          for J := 0 to Coordinates[I].Count - 1 do
            WriteStr(
              '      <point X="' +
              FloatToStr(Coordinates[I].Points[J].X) + '" Y="' +
              FloatToStr(Coordinates[I].Points[J].Y) + '" Z="' +
              FloatToStr(Coordinates[I].Points[J].Z) + '" />'#13#10, F
            );
          WriteStr('    </points>'#13#10, F);
        end;

      WriteStr('  </coordinates>'#13#10, F);
      WriteStr('</test>'#13#10, F);
      SysUtils.FileClose(F);
    end;
end;

end.
