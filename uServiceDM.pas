unit uServiceDM;

interface

uses
  SysUtils, Classes, Dialogs, ExtCtrls, ImgList, Controls,
  uConsole_1,
  ifpiclass, ifpiclassruntime, ifpiir_MCFile, ifpii_MCFile, XPMan, GLScene, GLGeomObjects,
  GLObjects, GLGraph, GLMisc, GLExtrusion, GLTexture,
  uMCPoint, uMCCounter;

type
  TfServiceDM = class(TDataModule)
    ScriptCompiler1: TScriptCompiler;
    ImgPreview: TImageList;
    ImgStartStop: TImageList;
    AnimateTimer: TTimer;
    CD: TColorDialog;
    XPManifest1: TXPManifest;
    GLScene1: TGLScene;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLGrid: TGLXYZGrid;
    GLCam1: TGLDummyCube;
    GLSphere1: TGLSphere;
    GLCylinder1: TGLCylinder;
    GLCone1: TGLCone;
    GLCam2: TGLDummyCube;
    GLSphere2: TGLSphere;
    GLCylinder2: TGLCylinder;
    GLCone2: TGLCone;
    GLCameraMain: TGLCamera;
    GLCameraCamView: TGLCamera;
    GLTripod1: TGLDummyCube;
    GLPipe1: TGLPipe;
    GLPipe2: TGLPipe;
    GLPipe3: TGLPipe;
    GLTripod2: TGLDummyCube;
    GLPipe4: TGLPipe;
    GLPipe5: TGLPipe;
    GLPipe6: TGLPipe;
    procedure AnimateTimerTimer(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure ScriptCompiler1AfterExecImport(Sender: TObject; x: TIFPSRuntimeClassImporter);
    procedure ScriptCompiler1AfterCompImport(Sender: TObject; x: TIFPSCompileTimeClassesImporter);
    procedure DataModuleCreate(Sender: TObject);
  private
    FMCFile: TMCFile;
    FCounter: TCounter;
    procedure DoOnLoadMCFile(Sender: TObject);
  public
    function GetFileList(const Path, Mask: String): TStrings;
    function GetFileNameByMask: String;
    function HexToInt(Hex: String): Integer;
    function HexToGlColor(Hex: String): TGLColor;
    function IsDigit(const C: Char): Boolean;
    function IsColor(const C: Char): Boolean;
    function IsFloat(const C: Char; const S: String): Boolean;
    property MCFile: TMCFile read FMCFile;
    property MCCounter: TCounter read FCounter;
  end;

var
  fServiceDM: TfServiceDM;

const
  PROGRAM_NAME = '3DMotionCheck';
  VERSION = '2.0';

implementation

uses
  Math, IFPS3CompExec, Forms,
  uParams;

{$R *.dfm}

procedure TfServiceDM.AnimateTimerTimer(Sender: TObject);
begin
  GLCameraMain.MoveAroundTarget(0, 0.5);
end;

procedure TfServiceDM.ScriptCompiler1AfterCompImport(Sender: TObject; x: TIFPSCompileTimeClassesImporter);
begin
  SIRegister_Parser(x);
end;

procedure TfServiceDM.ScriptCompiler1AfterExecImport(Sender: TObject; x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Parser(x);
end;

procedure TfServiceDM.DataModuleCreate(Sender: TObject);
begin
  with ScriptCompiler1.CompExec do
    begin
      AddFunction(@Cot, 'function Cot(const X: Double): Double;');
      AddFunction(@Tan, 'function Tan(const X: Double): Double;');
      AddFunction(@ArcCos, 'function ArcCos(const X : Double) : Double;');
      AddFunction(@ArcSin, 'function ArcSin(const X : Double) : Double;');
      AddFunction(@ArcTan, 'function ArcTan(const X : Double) : Double;');
      AddFunction(@ArcCot, 'function ArcCot(const X : Double) : Double;');
    end;
  Application.Title := PROGRAM_NAME + ' ' + Version;

  FMCFile := TMCFile.Create;
  FMCFile.OnLoadFile := DoOnLoadMCFile;
end;

procedure TfServiceDM.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(FCounter) then
    FreeAndNil(FCounter);
  FreeAndNil(FMCFile);
end;

procedure TfServiceDM.DoOnLoadMCFile(Sender: TObject);
begin
  if Assigned(FCounter) then
    FreeAndNil(FCounter);
  FCounter := TCounter.Create(FMCFile);
end;

function TfServiceDM.GetFileList(const Path, Mask: String): TStrings;
var
  Sr: TSearchRec;
begin
  Result := TStringList.Create;
  if FindFirst(Path + Mask, faAnyFile and not faDirectory, Sr) = 0 then
    begin
      repeat
        Result.Add(Sr.Name);
      until FindNext(Sr) <> 0;
      FindClose(Sr);
    end;
end;

function TfServiceDM.HexToGlColor(Hex: String): TGLColor;
begin
  Hex := StringReplace(AnsiUpperCase(Hex), '#', '', [rfReplaceAll]);
  Result := TGLColor.Create(nil);
  Result.Red := HexToInt(Copy(Hex, 1, 2)) / 255;
  Result.Green := HexToInt(Copy(Hex, 3, 2)) / 255;
  Result.Blue := HexToInt(Copy(Hex, 5, 2)) / 255;
end;

function TfServiceDM.HexToInt(Hex: String): Integer;
var i, Len, Value: Integer;
begin
  Hex := AnsiUpperCase(Hex);
  Len := Length(Hex);
  Result := 0;
  for i := 1 to Len do
    begin
      Value := 0;
      case Hex[i] of
        '0'..'9': Value := StrToInt(Hex[i]);
        'A'..'F': Value := Ord(Hex[i]) - 55;
      end;
      Result := Result + Value * Round(Power(16, Len - i));
    end;
end;

function TfServiceDM.IsColor(const C: Char): Boolean;
begin
  Result := UpCase(C) in ['0'..'9', 'A'..'F', '#', #8];
end;

function TfServiceDM.IsDigit(const C: Char): Boolean;
begin
  Result := C in ['0'..'9', #8];
end;

function TfServiceDM.IsFloat(const C: Char; const S: String): Boolean;
begin
  Result := (C in ['0'..'9', '-', #8]) or ((C = ',') and (Pos(',', S) = -1));
end;

function TfServiceDM.GetFileNameByMask: String;
begin
  Result:= Params['TestName'].AsString;
  Result := StringReplace(Result, '%NUM%', Params['LastTest'].AsString, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%DATE%', FormatDateTime('dd-mm-yyyy', Now), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%TIME%', FormatDateTime('hh-nn-ss', Now), [rfReplaceAll, rfIgnoreCase]);
end;

end.
