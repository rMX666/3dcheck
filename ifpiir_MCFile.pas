unit ifpiir_MCFile;

interface

uses
  ifps3, ifps3common, ifps3utl, ifpiclassruntime;
  
procedure RIRegister_Parser(cl: TIFPSRuntimeClassImporter);

procedure RIRegisterTMCPoint(cl: TIFPSRuntimeClassImporter);
procedure RIRegisterTParam(cl: TIFPSRuntimeClassImporter);
procedure RIRegisterTMCFile(cl: TIFPSRuntimeClassImporter);
procedure RIRegisterTCountParameterNameWraper(cl: TIFPSRuntimeClassImporter);
procedure RIRegisterTCounter(cl: TIFPSRuntimeClassImporter);

implementation

uses
  uMCPoint, uParams, uMCCounter;

procedure RIRegister_Parser(cl: TIFPSRuntimeClassImporter);
begin
  RIRegisterTMCPoint(cl);
  RIRegisterTParam(cl);
  RIRegisterTMCFile(cl);
  RIRegisterTCountParameterNameWraper(cl);
  RIRegisterTCounter(cl);
end;

procedure TMCPointPointsR(Self: TMCPoint; var T: T3DPoint; I: Integer); begin T := Self.Points[I]; end;
procedure TMCPointTimeR(Self: TMCPoint; var T: Integer); begin T := Self.Time; end;
procedure TMCPointCountR(Self: TMCPoint; var T: Integer); begin T := Self.Count; end;
procedure TMCPointPointsMetricR(Self: TMCPoint; var T: T3DPoint; I: Integer); begin T := Self.PointsMetric[I]; end;

procedure RIRegisterTMCPoint(cl: TIFPSRuntimeClassImporter);
begin
  with cl.Add(TMCPoint) do
    begin
      RegisterMethod(@TMCPoint.Clear, 'CLEAR');
      RegisterMethod(@TMCPoint.Add, 'ADD');
      RegisterPropertyHelper(@TMCPointPointsR, nil, 'POINTS');
      RegisterPropertyHelper(@TMCPointTimeR, nil, 'TIME');
      RegisterPropertyHelper(@TMCPointCountR, nil, 'COUNT');
      RegisterPropertyHelper(@TMCPointPointsMetricR, nil, 'POINTSMETRIC');
    end;
end;

procedure TParamNameR(Self: TParam; var T: String); begin T := Self.Name; end;
procedure TParamNameW(Self: TParam; var T: String); begin Self.Name := T; end;
procedure TParamAsFloatR(Self: TParam; var T: Double); begin T := Self.AsFloat; end;
procedure TParamAsFloatW(Self: TParam; var T: Double); begin Self.AsFloat := T; end;
procedure TParamAsIntegerR(Self: TParam; var T: Integer); begin T := Self.AsInteger; end;
procedure TParamAsIntegerW(Self: TParam; var T: Integer); begin Self.AsInteger := T; end;
procedure TParamAsStringR(Self: TParam; var T: String); begin T := Self.AsString; end;
procedure TParamAsStringW(Self: TParam; var T: String); begin Self.AsString := T; end;
procedure TParamAsBooleanR(Self: TParam; var T: Boolean); begin T := Self.AsBoolean; end;
procedure TParamAsBooleanW(Self: TParam; var T: Boolean); begin Self.AsBoolean := T; end;

procedure RIRegisterTParam(cl: TIFPSRuntimeClassImporter);
begin
  with cl.Add(TParam) do
    begin
      RegisterPropertyHelper(@TParamNameR, @TParamNameW, 'NAME');
      RegisterPropertyHelper(@TParamAsFloatR, @TParamAsFloatW, 'ASFLOAT');
      RegisterPropertyHelper(@TParamAsIntegerR, @TParamAsIntegerW, 'ASINTEGER');
      RegisterPropertyHelper(@TParamAsStringR, @TParamAsStringW, 'ASSTRING');
      RegisterPropertyHelper(@TParamAsBooleanR, @TParamAsBooleanW, 'ASBOOLEAN');
    end;
end;

procedure TMCFileCoordinatesR(Self: TMCFile; var T: TMCPoint; var I: Integer); begin T := Self.Coordinates[I]; end;
procedure TMCFileCoordinateCountR(Self: TMCFile; var T: Integer); begin T := Self.CoordinateCount; end;
procedure TMCFileOptionsR(Self: TMCFile; var T: TParam; var S: String); begin T := Self.Options[String(PChar(@S))]; end;

procedure RIRegisterTMCFile(cl: TIFPSRuntimeClassImporter);
begin
  with cl.Add(TMCFile) do
    begin
      RegisterConstructor(@TMCFile.Create, 'CREATE');
      RegisterMethod(@TMCFile.Clear, 'CLEAR');
      RegisterMethod(@TMCFile.LoadFile, 'LOADFILE');
      RegisterMethod(@TMCFile.SaveFile, 'SAVEFILE');
      RegisterMethod(@TMCFile.AddCoordinate, 'ADDCOORDINATE');
      RegisterPropertyHelper(@TMCFileCoordinatesR, nil, 'COORDINATES');
      RegisterPropertyHelper(@TMCFileCoordinateCountR, nil, 'COORDINATECOUNT');
      RegisterPropertyHelper(@TMCFileOptionsR, nil, 'OPTIONS');
    end;
end;

procedure TCountParameterNameWraperCountR(Self: TCountParameterNameWraper; var T: Integer); begin T := Self.Count; end;
procedure TCountParameterNameWraperNameR(Self: TCountParameterNameWraper; var T: String; I: Integer); begin T := Self.Name[I]; end;
procedure TCountParameterNameWraperNameW(Self: TCountParameterNameWraper; var T: String; I: Integer); begin Self.Name[I] := T; end;

procedure RIRegisterTCountParameterNameWraper(cl: TIFPSRuntimeClassImporter);
begin
  with cl.Add(TCountParameterNameWraper) do
    begin
      RegisterConstructor(@TCountParameterNameWraper.Create, 'CREATE');
      RegisterMethod(@TCountParameterNameWraper.Add, 'ADD');
      RegisterPropertyHelper(@TCountParameterNameWraperCountR, nil, 'COUNT');
      RegisterPropertyHelper(@TCountParameterNameWraperNameR, @TCountParameterNameWraperNameW, 'NAME');
    end;
end;

procedure TCounterTypeCountR(Self: TCounter; var T: Integer); begin T := Self.TypeCount; end;
procedure TCounterValueR(Self: TCounter; var T: Double; I: Integer; I1: Integer; I2: Integer); begin T := Self.Value[I, I1, I2]; end;

procedure RIRegisterTCounter(cl: TIFPSRuntimeClassImporter);
begin
  with cl.Add(TCounter) do
    begin
      RegisterConstructor(@TCounter.Create, 'CREATE');
      RegisterMethod(@TCounter.GetNameWrapper, 'GETNAMEWRAPPER');
      RegisterPropertyHelper(@TCounterTypeCountR, nil, 'TYPECOUNT');
      RegisterPropertyHelper(@TCounterValueR, nil, 'VALUE');
    end;
end;

end.