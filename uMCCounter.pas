unit uMCCounter;

interface

uses
  SysUtils, Classes, uMCPoint;

type
  TCountParameterMetaType = (cpatNone, cpatStandart, cpatCinamatic, cpatDinamic, cpatAngle);

  TCountParameterType = (
    cpaDeltaTime, cpaX, cpaY, cpaZ,       // Standart parameters

    cpaPathX, cpaPathY, cpaPathZ, cpaPathMod,     // Cinamatic parameters
    cpaSpeedX, cpaSpeedY, cpaSpeedZ, cpaSpeedMod,
    cpaAccelX, cpaAccelY, cpaAccelZ, cpaAccelMod,

    cpaPulseX, cpaPulseY, cpaPulseZ, cpaPulseMod, // Dinamic paramaters
    cpaForceX, cpaForceY, cpaForceZ, cpaForceMod,
    cpaWork,

    cpaAnglePath, cpaAngleAxeX, cpaAngleAxeY, cpaAngleAxeZ, // Angle paramaters
    cpaCircleRadius
  );

type
  TCountParameterNameWraper = class
  private
    FNames: TStrings;
    procedure SetName(const Index: Integer; const Value: String);
    function GetCount: Integer;
    function GetName(const Index: Integer): String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name: String);
    property Count: Integer read GetCount;
    property Name[const Index: Integer]: String read GetName write SetName; default;
  end;

  TCountParameterNameWrapperList = class
  private
    FWrappers: TList;
    function GetWraper(const Index: Integer): TCountParameterNameWraper;
  public
    constructor Create;
    destructor Destroy; override;
    property Wrapper[const Index: Integer]: TCountParameterNameWraper read GetWraper; default;
  end;

type
  TCounter = class;

  TCountHandler = function(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real of object;
  TCountHandlers = array of TCountHandler;

  THandlerCacheItem = record
    Cached: Boolean;
    Value: Real;
  end;

  THandlerCache = class
  private
    FCache: array [TCountParameterType] of array of array of THandlerCacheItem;
    function GetValue(const AType: TCountParameterType; const Index, PointIndex: Integer): Real;
    procedure SetValue(const AType: TCountParameterType; const Index, PointIndex: Integer; const Value: Real);
  public
    constructor Create(const AOwner: TMCFile);
    function IsCached(const AType: TCountParameterType; const Index, PointIndex: Integer): Boolean;
    property Value[const AType: TCountParameterType; const Index, PointIndex: Integer]: Real read GetValue write SetValue;
  end;

  TDefaultHandler = class
  private
    FOwner: TCounter;
    FHandlers: array [TCountParameterType] of TCountHandler;
    FCache: THandlerCache;
    function GetHandler(const Index: TCountParameterType): TCountHandler;
    function CheckCoordinatePoint(const AOwner: TCounter; const Index, PointIndex: Integer;
      const ExceptionText: String = ''): Boolean;
    procedure RaiseListErrorFmt(const Text: String; const Args: array of const);
  public
    constructor Create(const AOwner: TCounter);
    destructor Destroy; override;

    function GetdT(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetX(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetY(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetZ(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetSx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetSy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetSz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetSmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getvx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getvy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getvz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getvmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getax(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getay(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getaz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function Getamod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetPx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetPy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetPz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetPmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetFx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetFy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetFz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetFmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetA(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetAngleS(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetAngleX(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetAngleY(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetAngleZ(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
    function GetRadius(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;

    property Handlers[const Index: TCountParameterType]: TCountHandler read GetHandler;
  end;

  TCounter = class
  private
    FWrappers: TCountParameterNameWrapperList;
    FOwner: TMCFile;
    FDefHandler: TDefaultHandler;
    FHandlers: TCountHandlers;
    function GetCountParameterMeta(const AType: TCountParameterType): TCountParameterMetaType;
    function GetTypeCount: Integer;
    function GetValue(const AType, Index, PointIndex: Integer): Real;
    procedure InitHandlers;
  public
    constructor Create(const AOwner: TMCFile);
    destructor Destroy; override;
    function GetNameWrapper(const Index: Integer): TCountParameterNameWraper;
    property TypeCount: Integer read GetTypeCount;
    property Value[const AType, Index, PointIndex: Integer]: Real read GetValue;
    property CountParameterMeta[const AType: TCountParameterType]: TCountParameterMetaType read GetCountParameterMeta;
  end;

implementation

uses
  Math, uConst;

{ THandlerCache }

constructor THandlerCache.Create(const AOwner: TMCFile);
var
  I: TCountParameterType;
  J, K: Integer;
begin
  for I := Low(TCountParameterType) to High(TCountParameterType) do
    begin
      SetLength(FCache[I], AOwner.CoordinateCount);
      for J := 0 to AOwner.CoordinateCount - 1 do
        begin
          SetLength(FCache[I, J], AOwner.Options['PointCount'].AsInteger);
          for K := 0 to AOwner.Options['PointCount'].AsInteger - 1 do
            begin
              FCache[I, J, K].Cached := False;
              FCache[I, J, K].Value := 0;
            end;
        end;
    end;
end;

function THandlerCache.GetValue(const AType: TCountParameterType; const Index, PointIndex: Integer): Real;
begin
  Result := FCache[AType, Index, PointIndex].Value;
end;

function THandlerCache.IsCached(const AType: TCountParameterType; const Index, PointIndex: Integer): Boolean;
begin
  Result := FCache[AType, Index, PointIndex].Cached;
end;

procedure THandlerCache.SetValue(const AType: TCountParameterType; const Index, PointIndex: Integer; const Value: Real);
begin
  FCache[AType, Index, PointIndex].Cached := True;
  FCache[AType, Index, PointIndex].Value := Value;
end;

{ TDefaultHandler }

function TDefaultHandler.CheckCoordinatePoint(const AOwner: TCounter; const Index, PointIndex: Integer;
  const ExceptionText: String): Boolean;
begin
  with AOwner.FOwner do
    begin
      Result := InRange(Index, 0, CoordinateCount - 1) and InRange(PointIndex, 0, Options['PointCount'].AsInteger - 1);
      if not Result and (ExceptionText <> '') then
        RaiseListErrorFmt(ExceptionText, [Index, CoordinateCount - 1, PointIndex, Options['PointCount'].AsInteger - 1]);
    end;
end;

constructor TDefaultHandler.Create(const AOwner: TCounter);
begin
  FOwner := AOwner;

  FCache := THandlerCache.Create(FOwner.FOwner);

  FHandlers[cpaDeltaTime] := GetdT;
  FHandlers[cpaX] := GetX;
  FHandlers[cpaY] := GetY;
  FHandlers[cpaZ] := GetZ;
  FHandlers[cpaPathX] := GetSx;
  FHandlers[cpaPathY] := GetSy;
  FHandlers[cpaPathZ] := GetSz;
  FHandlers[cpaPathMod] := GetSmod;
  FHandlers[cpaSpeedX] := Getvx;
  FHandlers[cpaSpeedY] := Getvy;
  FHandlers[cpaSpeedZ] := Getvz;
  FHandlers[cpaSpeedMod] := Getvmod;
  FHandlers[cpaAccelX] := Getax;
  FHandlers[cpaAccelY] := Getay;
  FHandlers[cpaAccelZ] := Getaz;
  FHandlers[cpaAccelMod] := Getamod;
  FHandlers[cpaPulseX] := GetPx;
  FHandlers[cpaPulseY] := GetPy;
  FHandlers[cpaPulseZ] := GetPz;
  FHandlers[cpaPulseMod] := GetPmod;
  FHandlers[cpaForceX] := GetFx;
  FHandlers[cpaForceY] := GetFy;
  FHandlers[cpaForceZ] := GetFz;
  FHandlers[cpaForceMod] := GetFmod;
  FHandlers[cpaWork] := GetA;
  FHandlers[cpaAnglePath] := GetAngleS;
  FHandlers[cpaAngleAxeX] := GetAngleX;
  FHandlers[cpaAngleAxeY] := GetAngleY;
  FHandlers[cpaAngleAxeZ] := GetAngleZ;
  FHandlers[cpaCircleRadius] := GetRadius;
end;

destructor TDefaultHandler.Destroy;
begin
  FreeAndNil(FCache);
  FOwner := nil;
  inherited;
end;

function TDefaultHandler.GetHandler(const Index: TCountParameterType): TCountHandler;
begin
  Result := FHandlers[Index];
end;

function TDefaultHandler.GetA(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaWork, Index, PointIndex) then
    begin
      FCache.Value[cpaWork, Index, PointIndex] := GetFx(AOwner, Index, PointIndex) * GetSx(AOwner, Index, PointIndex) +
        GetFy(AOwner, Index, PointIndex) * GetSy(AOwner, Index, PointIndex) +
        GetFz(AOwner, Index, PointIndex) * GetSz(AOwner, Index, PointIndex);
    end;
  Result := FCache.Value[cpaWork, Index, PointIndex];
end;

function TDefaultHandler.Getamod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
var
  ax, ay, az: Real;
begin
  if not FCache.IsCached(cpaAccelMod, Index, PointIndex) then
    begin
      ax := Getax(AOwner, Index, PointIndex);
      ay := Getay(AOwner, Index, PointIndex);
      az := Getaz(AOwner, Index, PointIndex);
      FCache.Value[cpaAccelMod, Index, PointIndex] := Sqrt(Sqr(ax) + Sqr(ay) + Sqr(az));
    end;
  Result := FCache.Value[cpaAccelMod, Index, PointIndex];
end;

function TDefaultHandler.GetAngleS(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAnglePath, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 1, PointIndex) then
        FCache.Value[cpaAnglePath, Index, PointIndex] :=
          (GetSx(AOwner, Index, PointIndex) * GetSx(AOwner, Index - 1, PointIndex) +
          GetSy(AOwner, Index, PointIndex) * GetSy(AOwner, Index - 1, PointIndex) +
          GetSz(AOwner, Index, PointIndex) * GetSz(AOwner, Index - 1, PointIndex)) /
          (GetSmod(AOwner, Index, PointIndex) * GetSmod(AOwner, Index - 1, PointIndex))
      else
        FCache.Value[cpaAnglePath, Index, PointIndex] := 0;
    end;
  Result := FCache.Value[cpaAnglePath, Index, PointIndex];
end;

function TDefaultHandler.GetAngleX(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAngleAxeX, Index, PointIndex) then
    FCache.Value[cpaAngleAxeX, Index, PointIndex] := (180 / Pi) * ArcCos(GetSx(AOwner, Index, PointIndex) / GetSmod(AOwner, Index, PointIndex));

  Result := FCache.Value[cpaAngleAxeX, Index, PointIndex];
end;

function TDefaultHandler.GetAngleY(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAngleAxeY, Index, PointIndex) then
    FCache.Value[cpaAngleAxeY, Index, PointIndex] := (180 / Pi) * ArcCos(GetSy(AOwner, Index, PointIndex) / GetSmod(AOwner, Index, PointIndex));

  Result := FCache.Value[cpaAngleAxeY, Index, PointIndex];
end;

function TDefaultHandler.GetAngleZ(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAngleAxeZ, Index, PointIndex) then
    FCache.Value[cpaAngleAxeZ, Index, PointIndex] := (180 / Pi) * ArcCos(GetSz(AOwner, Index, PointIndex) / GetSmod(AOwner, Index, PointIndex));

  Result := FCache.Value[cpaAngleAxeZ, Index, PointIndex];
end;

function TDefaultHandler.Getax(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAccelX, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 2, PointIndex) then
        FCache.Value[cpaAccelX, Index, PointIndex] := Getvx(AOwner, Index, PointIndex) - Getvx(AOwner, Index - 1, PointIndex) / GetdT(AOwner, Index - 2, PointIndex)
      else
        FCache.Value[cpaAccelX, Index, PointIndex] := 0;
    end;
  Result := FCache.Value[cpaAccelX, Index, PointIndex];
end;

function TDefaultHandler.Getay(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAccelY, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 2, PointIndex) then
        FCache.Value[cpaAccelY, Index, PointIndex] := Getvy(AOwner, Index, PointIndex) - Getvy(AOwner, Index - 1, PointIndex) / GetdT(AOwner, Index - 2, PointIndex)
      else
        FCache.Value[cpaAccelY, Index, PointIndex] := 0;
    end;
  Result := FCache.Value[cpaAccelY, Index, PointIndex];
end;

function TDefaultHandler.Getaz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaAccelZ, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 2, PointIndex) then
        FCache.Value[cpaAccelZ, Index, PointIndex] := Getvz(AOwner, Index, PointIndex) - Getvz(AOwner, Index - 1, PointIndex) / GetdT(AOwner, Index - 2, PointIndex)
      else
        FCache.Value[cpaAccelZ, Index, PointIndex] := 0;
    end;
  Result := FCache.Value[cpaAccelZ, Index, PointIndex];
end;

function TDefaultHandler.GetdT(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaDeltaTime, Index, PointIndex) then
    begin
      with AOwner.FOwner do
        if CheckCoordinatePoint(AOwner, Index, PointIndex, cOutOfRangeCoordinate) then
          FCache.Value[cpaDeltaTime, Index, PointIndex] := Coordinates[Index].Time / 1000;
    end;
  Result := FCache.Value[cpaDeltaTime, Index, PointIndex];
end;

function TDefaultHandler.GetFmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
var
  Fx, Fy, Fz: Real;
begin
  if not FCache.IsCached(cpaForceMod, Index, PointIndex) then
    begin
      Fx := GetFx(AOwner, Index, PointIndex);
      Fy := GetFy(AOwner, Index, PointIndex);
      Fz := GetFz(AOwner, Index, PointIndex);
      FCache.Value[cpaForceMod, Index, PointIndex] := Sqrt(Sqr(Fx) + Sqr(Fy) + Sqr(Fz));
    end;
  Result := FCache.Value[cpaForceMod, Index, PointIndex];
end;

function TDefaultHandler.GetFx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaForceX, Index, PointIndex) then
    FCache.Value[cpaForceX, Index, PointIndex] := Getax(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaForceX, Index, PointIndex];
end;

function TDefaultHandler.GetFy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaForceY, Index, PointIndex) then
    FCache.Value[cpaForceY, Index, PointIndex] := Getay(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaForceY, Index, PointIndex];
end;

function TDefaultHandler.GetFz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaForceZ, Index, PointIndex) then
    FCache.Value[cpaForceZ, Index, PointIndex] := Getaz(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaForceZ, Index, PointIndex];
end;

function TDefaultHandler.GetPmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
var
  Px, Py, Pz: Real;
begin
  if not FCache.IsCached(cpaPulseMod, Index, PointIndex) then
    begin
      Px := GetPx(AOwner, Index, PointIndex);
      Py := GetPy(AOwner, Index, PointIndex);
      Pz := GetPz(AOwner, Index, PointIndex);
      FCache.Value[cpaPulseMod, Index, PointIndex] := Sqrt(Sqr(Px) + Sqr(Py) + Sqr(Pz));
    end;
  Result := FCache.Value[cpaPulseMod, Index, PointIndex];
end;

function TDefaultHandler.GetPx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPulseX, Index, PointIndex) then
    FCache.Value[cpaPulseX, Index, PointIndex] := Getvx(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaPulseX, Index, PointIndex];
end;

function TDefaultHandler.GetPy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPulseY, Index, PointIndex) then
    FCache.Value[cpaPulseY, Index, PointIndex] := Getvy(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaPulseY, Index, PointIndex];
end;

function TDefaultHandler.GetPz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPulseZ, Index, PointIndex) then
    FCache.Value[cpaPulseZ, Index, PointIndex] := Getvz(AOwner, Index, PointIndex) * AOwner.FOwner.Options['Mass'].AsFloat;

  Result := FCache.Value[cpaPulseZ, Index, PointIndex];
end;

//______________________________________________________________________________________________________________________
// Find circle radius
function TDefaultHandler.GetRadius(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;

const
  N = 3;

type
  TMatrix33 = array [0..2, 0..2] of Real;
  TMatrix22 = array [0..1, 0..1] of Real;
  TVector = array [0..2] of Real;

function GetInput(var A: TMatrix33): Boolean;
begin
  Result := (CheckCoordinatePoint(AOwner, Index, PointIndex) and
    CheckCoordinatePoint(AOwner, Index - 1, PointIndex) and
    CheckCoordinatePoint(AOwner, Index + 1, PointIndex));
  if not Result then
    Exit;

  // X
  A[0,0] := GetX(AOwner, Index - 1, PointIndex);
  A[0,1] := GetX(AOwner, Index, PointIndex);
  A[0,2] := GetX(AOwner, Index + 1, PointIndex);

  // Y
  A[1,0] := GetY(AOwner, Index - 1, PointIndex);
  A[1,1] := GetY(AOwner, Index, PointIndex);
  A[1,2] := GetY(AOwner, Index + 1, PointIndex);

  // Z
  A[2,0] := GetZ(AOwner, Index - 1, PointIndex);
  A[2,1] := GetZ(AOwner, Index, PointIndex);
  A[2,2] := GetZ(AOwner, Index + 1, PointIndex);
end;

function FindA(const I: TMatrix33): TMatrix33;
begin
  // 1 string
  Result[0,0] := I[0,1] - I[0,0];
  Result[0,1] := I[1,1] - I[1,0];
  Result[0,2] := I[2,1] - I[2,0];

  // 2 string
  Result[1,0] := I[0,2] - I[0,1];
  Result[1,1] := I[1,2] - I[1,1];
  Result[1,2] := I[2,2] - I[2,1];

  // 2 string
  Result[2,0] := (I[1,0] - I[1,1]) * (I[2,0] - I[2,2]) - (I[1,0] - I[1,2]) * (I[2,0] - I[2,1]);
  Result[2,1] := (I[0,0] - I[0,2]) * (I[2,0] - I[2,1]) - (I[0,0] - I[0,1]) * (I[2,0] - I[2,2]);
  Result[2,2] := (I[0,0] - I[0,1]) * (I[1,0] - I[1,2]) - (I[0,0] - I[0,2]) * (I[1,0] - I[1,1]);
end;

function FindB(const I: TMatrix33): TVector;
begin
  Result[0] := (Sqr(I[0,1]) - Sqr(I[0,0]) + Sqr(I[1,1]) - Sqr(I[1,0]) + Sqr(I[2,1]) - Sqr(I[2,0])) / 2;
  Result[1] := (Sqr(I[0,2]) - Sqr(I[0,1]) + Sqr(I[1,2]) - Sqr(I[1,1]) + Sqr(I[2,2]) - Sqr(I[2,1])) / 2;
  Result[2] := I[0,0] * ((I[1,0] - I[1,1]) * (I[2,0] - I[2,2]) - (I[1,0] - I[1,2]) * (I[2,0] - I[2,1])) +
               I[1,0] * ((I[0,0] - I[0,2]) * (I[2,0] - I[2,1]) - (I[0,0] - I[0,1]) * (I[2,0] - I[2,2])) +
               I[2,0] * ((I[0,0] - I[0,1]) * (I[1,0] - I[1,2]) - (I[0,0] - I[0,2]) * (I[1,0] - I[1,1]));
end;

function FindDetA(const A: TMatrix33): Real;
begin
  Result := A[0,0] * (A[1,1] * A[2,2] - A[1,2] * A[2,1]) -
            A[1,0] * (A[0,1] * A[2,2] - A[0,2] * A[2,1]) +
            A[2,0] * (A[0,1] * A[1,2] - A[0,2] * A[1,1]);
end;

function FindDet22(const A: TMatrix22): Real;
begin
  Result := A[0, 0] * A[1, 1] - A[0, 1] * A[1, 0];
end;

function FindUnA(const A: TMatrix33; const DetA: Real): TMatrix33;
var
  i1, j1, i, j, k1, k2: Integer;
  Dop: TMatrix22;
  DetDop: Real;
begin
  for i1 := 0 to N - 1 do
    for j1 := 0 to N - 1 do
		  begin
			  k1 := 0;
        for i := 0 to N - 1 do
				  begin
						if (i = i1) then
							Continue;
						k2 := 0;
						for j := 0 to N - 1 do
							begin
								if (j = j1) then
									Continue;
								Dop[k1, k2] := A[i, j];
								k2 := k2 + 1;
							end;
						k1 := k1 + 1;
					end;
					// Находим определитель
					DetDop := FindDet22(Dop);
					if ((i1 + j1) mod 2 <> 0) then
						DetDop := -DetDop;
					Result[j1,i1] := DetDop / DetDop;
			end;
end;

function Multiply(const A: TMatrix33; const B: TVector): TVector;
var
  I, J: Integer;
begin
 	for I := 0 to N - 1 do
  	begin
	  	Result[I] := 0;
		  for J := 0 to N - 1 do
			  Result[I] := Result[I] + A[I, J] * B[J];
    end;
end;

var
  Tmp, A, UnA: TMatrix33;
  B, Center: TVector;
  DetA: Real;
begin
  if not FCache.IsCached(cpaCircleRadius, Index, PointIndex) then
    begin
      if GetInput(Tmp) then
        begin
          A := FindA(Tmp);
          B := FindB(Tmp);
          DetA := FindDetA(A);
          if DetA <> 0 then
            begin
              UnA := FindUnA(A, DetA);
              Center := Multiply(UnA, B);
              FCache.Value[cpaCircleRadius, Index, PointIndex] := Sqrt(Sqr(Center[0]) + Sqr(Center[1]) + Sqr(Center[2]));
            end
          else
            FCache.Value[cpaCircleRadius, Index, PointIndex] := 0;
        end
      else
        FCache.Value[cpaCircleRadius, Index, PointIndex] := 0;
    end;
  Result := FCache.Value[cpaCircleRadius, Index, PointIndex];
end;
// Find circle radius END!!

function TDefaultHandler.GetSmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
var
  Sx, Sy, Sz: Real;
begin
  if not FCache.IsCached(cpaPathMod, Index, PointIndex) then
    begin
      Sx := GetSx(AOwner, Index, PointIndex);
      Sy := GetSy(AOwner, Index, PointIndex);
      Sz := GetSz(AOwner, Index, PointIndex);
      FCache.Value[cpaPathMod, Index, PointIndex] := Sqrt(Sqr(Sx) + Sqr(Sy) + Sqr(Sz));
    end;
  Result := FCache.Value[cpaPathMod, Index, PointIndex];
end;

function TDefaultHandler.GetSx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPathX, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 1, PointIndex) then
        FCache.Value[cpaPathX, Index, PointIndex] := GetX(AOwner, Index, PointIndex) - GetX(AOwner, Index - 1, PointIndex)
      else
        FCache.Value[cpaPathX, Index, PointIndex] := GetX(AOwner, Index, 0);
    end;
  Result := FCache.Value[cpaPathX, Index, PointIndex];
end;

function TDefaultHandler.GetSy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPathY, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 1, PointIndex) then
        FCache.Value[cpaPathY, Index, PointIndex] := GetY(AOwner, Index, PointIndex) - GetY(AOwner, Index - 1, PointIndex)
      else
        FCache.Value[cpaPathY, Index, PointIndex] := GetY(AOwner, Index, 0);
    end;
  Result := FCache.Value[cpaPathY, Index, PointIndex];
end;

function TDefaultHandler.GetSz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaPathZ, Index, PointIndex) then
    begin
      if CheckCoordinatePoint(AOwner, Index - 1, PointIndex) then
        FCache.Value[cpaPathZ, Index, PointIndex] := GetZ(AOwner, Index, PointIndex) - GetZ(AOwner, Index - 1, PointIndex)
      else
        FCache.Value[cpaPathZ, Index, PointIndex] := GetZ(AOwner, Index, 0);
    end;
  Result := FCache.Value[cpaPathZ, Index, PointIndex];
end;

function TDefaultHandler.Getvmod(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
var
  vx, vy, vz: Real;
begin
  if not FCache.IsCached(cpaSpeedMod, Index, PointIndex) then
    begin
      vx := Getvx(AOwner, Index, PointIndex);
      vy := Getvy(AOwner, Index, PointIndex);
      vz := Getvz(AOwner, Index, PointIndex);
      FCache.Value[cpaSpeedMod, Index, PointIndex] := Sqrt(Sqr(vx) + Sqr(vy) + Sqr(vz));
    end;
  Result := FCache.Value[cpaSpeedMod, Index, PointIndex];
end;

function TDefaultHandler.Getvx(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaSpeedX, Index, PointIndex) then
    FCache.Value[cpaSpeedX, Index, PointIndex] := GetSx(AOwner, Index, PointIndex) / GetdT(AOwner, Index, PointIndex);

  Result := FCache.Value[cpaSpeedX, Index, PointIndex];
end;

function TDefaultHandler.Getvy(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaSpeedY, Index, PointIndex) then
    FCache.Value[cpaSpeedY, Index, PointIndex] := GetSy(AOwner, Index, PointIndex) / GetdT(AOwner, Index, PointIndex);

  Result := FCache.Value[cpaSpeedY, Index, PointIndex];
end;

function TDefaultHandler.Getvz(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaSpeedZ, Index, PointIndex) then
    FCache.Value[cpaSpeedZ, Index, PointIndex] := GetSz(AOwner, Index, PointIndex) / GetdT(AOwner, Index, PointIndex);

  Result := FCache.Value[cpaSpeedZ, Index, PointIndex];
end;

function TDefaultHandler.GetX(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaX, Index, PointIndex) then
    with AOwner.FOwner do
      if CheckCoordinatePoint(AOwner, Index, PointIndex, cOutOfRangeCoordinate) then
        FCache.Value[cpaX, Index, PointIndex] := Coordinates[Index].PointsMetric[PointIndex].X;

  Result := FCache.Value[cpaX, Index, PointIndex];
end;

function TDefaultHandler.GetY(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaY, Index, PointIndex) then
    with AOwner.FOwner do
      if CheckCoordinatePoint(AOwner, Index, PointIndex, cOutOfRangeCoordinate) then
        FCache.Value[cpaY, Index, PointIndex] := Coordinates[Index].PointsMetric[PointIndex].Y;

  Result := FCache.Value[cpaY, Index, PointIndex];
end;

function TDefaultHandler.GetZ(AOwner: TCounter; const Index: Integer; const PointIndex: Integer): Real;
begin
  if not FCache.IsCached(cpaZ, Index, PointIndex) then
    with AOwner.FOwner do
      if CheckCoordinatePoint(AOwner, Index, PointIndex, cOutOfRangeCoordinate) then
        FCache.Value[cpaZ, Index, PointIndex] := Coordinates[Index].PointsMetric[PointIndex].Z;

  Result := FCache.Value[cpaZ, Index, PointIndex];
end;

procedure TDefaultHandler.RaiseListErrorFmt(const Text: String; const Args: array of const);
begin
  raise EListError.CreateFmt(Text, Args);
end;

{ TCountParameterMetaTypeWraper }

procedure TCountParameterNameWraper.Add(const Name: String);
begin
  FNames.Add(Name);
end;

constructor TCountParameterNameWraper.Create;
begin
  FNames := TStringList.Create;
end;

destructor TCountParameterNameWraper.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

function TCountParameterNameWraper.GetCount: Integer;
begin
  Result := FNames.Count;
end;

function TCountParameterNameWraper.GetName(const Index: Integer): String;
begin
  Result := FNames[Index];
end;

procedure TCountParameterNameWraper.SetName(const Index: Integer; const Value: String);
begin
  FNames[Index] := Value;
end;

{ TCountParameterNameWrappers }

constructor TCountParameterNameWrapperList.Create;
begin
  FWrappers := TList.Create;

  FWrappers.Add(TCountParameterNameWraper.Create);
  with TCountParameterNameWraper(FWrappers[0]) do
    begin
      Add('Стандартные параметры');
      Add('Кинематические параметры');
      Add('Динамические параметры');
      Add('Угловые и криволинейные параметры');
    end;

  FWrappers.Add(TCountParameterNameWraper.Create);
  with TCountParameterNameWraper(FWrappers[1]) do
    begin
      Add('Время dT');
      Add('Координата X');
      Add('Координата Y');
      Add('Координата Z');

      Add('Путь Sx');
      Add('Путь Sy');
      Add('Путь Sz');
      Add('Путь Smod');
      Add('Скорость vx');
      Add('Скорость vy');
      Add('Скорость vz');
      Add('Скорость Vmod');
      Add('Ускорение ax');
      Add('Ускорение ay');
      Add('Ускорение az');
      Add('Ускорение amod');

      Add('Импульс силы Px');
      Add('Импульс силы Py');
      Add('Импульс силы Pz');
      Add('Импульс силы Pmod');
      Add('Сила Fx');
      Add('Сила Fy');
      Add('Сила Fz');
      Add('Сила Fmod');
      Add('Работа A');

      Add('Угол между векторами перемещения');
      Add('Угол между вектором перемещения и осью X');
      Add('Угол между вектором перемещения и осью Y');
      Add('Угол между вектором перемещения и осью Z');
      Add('Радиус окружности R');
    end;
end;

destructor TCountParameterNameWrapperList.Destroy;
var
  I: Integer;
begin
  for I := 0 to FWrappers.Count - 1 do
    TCountParameterNameWraper(FWrappers[I]).Free;
  FreeAndNil(FWrappers);
  inherited;
end;

function TCountParameterNameWrapperList.GetWraper(const Index: Integer): TCountParameterNameWraper;
begin
  if InRange(Index, 0, FWrappers.Count - 1) then
    Result := TCountParameterNameWraper(FWrappers[Index])
  else
    raise EListError.CreateFmt(cOutOfRangeWrappers, [Index, FWrappers.Count - 1]);
end;

{ TCounter }

constructor TCounter.Create(const AOwner: TMCFile);
begin
  FOwner := AOwner;
  FWrappers := TCountParameterNameWrapperList.Create;
  SetLength(FHandlers, Ord(High(TCountParameterType)) + 1);
  FDefHandler := TDefaultHandler.Create(Self);
  InitHandlers;
end;

destructor TCounter.Destroy;
begin
  FreeAndNil(FWrappers);
  FreeAndNil(FDefHandler);
  inherited;
end;

function TCounter.GetCountParameterMeta(const AType: TCountParameterType): TCountParameterMetaType;
begin
  case AType of
    cpaDeltaTime..cpaZ:
      Result := cpatStandart;
    cpaPathX..cpaAccelMod:
      Result := cpatCinamatic;
    cpaPulseX..cpaWork:
      Result := cpatDinamic;
    cpaAnglePath..cpaCircleRadius:
      Result := cpatAngle;
    else
      Result := cpatNone;
  end;
end;

function TCounter.GetNameWrapper(const Index: Integer): TCountParameterNameWraper;
begin
  Result := FWrappers[Index];
end;

function TCounter.GetTypeCount: Integer;
begin
  Result := Length(FHandlers);
end;

function TCounter.GetValue(const AType, Index, PointIndex: Integer): Real;
begin
  Result := FHandlers[AType](Self, Index, PointIndex);
end;

procedure TCounter.InitHandlers;
var
  I: TCountParameterType;
begin
  for I := Low(TCountParameterType) to High(TCountParameterType) do
    FHandlers[Integer(I)] := FDefHandler.Handlers[I];
end;

end.
