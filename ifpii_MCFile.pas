unit ifpii_MCFile;

{$I ifps3_def.inc}

interface

uses
  ifpscomp, ifps3common, ifps3utl, ifpiclass;
  
procedure SIRegister_Parser(cl: TIFPSCompileTimeClassesImporter);

procedure SIRegisterT3DPoint(cl: TIFPSCompileTimeClassesImporter);
procedure SIRegisterTMCPoint(cl: TIFPSCompileTimeClassesImporter);
procedure SIRegisterTParam(cl: TIFPSCompileTimeClassesImporter);
procedure SIRegisterTMCFile(cl: TIFPSCompileTimeClassesImporter);

implementation

uses
  uMCPoint, uParams;

procedure SIRegisterT3DPoint(cl: TIFPSCompileTimeClassesImporter);
begin
  cl.SE.AddTypeS('T3DPoint', 'Record X, Y, Z: Double; End;');
end;

procedure SIRegisterTMCPoint(cl: TIFPSCompileTimeClassesImporter);
begin
  with Cl.Add(cl.FindClass('TObject'), TMCPoint) do
    begin
      RegisterMethod('procedure Clear;');
      RegisterMethod('procedure Add(X, Y, Z: Double);');
      RegisterProperty('Points', 'T3DPoint Integer', iptR);
      RegisterProperty('Time', 'Integer', iptRW);
      RegisterProperty('Count', 'Integer', iptR);
      RegisterProperty('PointsMetric', 'T3DPoint Integer', iptR);
    end;
end;

procedure SIRegisterTParam(cl: TIFPSCompileTimeClassesImporter);
begin
  with Cl.Add(cl.FindClass('TObject'), TParam) do
    begin
      RegisterProperty('Name', 'String', iptRW);
      RegisterProperty('AsFloat', 'Double', iptRW);
      RegisterProperty('AsInteger', 'Integer', iptRW);
      RegisterProperty('AsString', 'String', iptRW);
      RegisterProperty('AsBoolean', 'Boolean', iptRW);
    end;
end;

procedure SIRegisterTMCFile(cl: TIFPSCompileTimeClassesImporter);
begin
  with Cl.Add(cl.FindClass('TObject'), TMCFile) do
    begin
      RegisterMethod('constructor Create;');
      RegisterMethod('procedure Clear;');
      RegisterMethod('procedure LoadFile(FileName: String);');
      RegisterMethod('procedure SaveFile(FileName: String);');
      RegisterMethod('function AddCoordinate: TMCPoint;');
      RegisterProperty('Coordinates', 'TMCPoint Integer', iptR);
      RegisterProperty('CoordinateCount', 'Integer', iptR);
      RegisterProperty('Options', 'TParam String', iptR);
    end;
end;

procedure SIRegister_Parser(cl: TIFPSCompileTimeClassesImporter);
begin
  SIRegisterT3DPoint(cl);
  SIRegisterTMCPoint(cl);
  SIRegisterTParam(cl);
  SIRegisterTMCFile(cl);
end;

end.