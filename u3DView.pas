unit u3DView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLWin32Viewer, GLScene, Buttons, Math, GLObjects;

type
  Tf3DView = class(TForm)
    GLViewerCamsForm: TGLSceneViewer;
    btn3DViewClose: TSpeedButton;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GLViewerCamsFormDblClick(Sender: TObject);
    procedure GLViewerCamsFormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure btn3DViewCloseClick(Sender: TObject);
  private
    FSceneLines: array of TGLLines;
    FSceneCubes: array of TGLDummyCube;
    procedure ClearScene;
    procedure ScenePlaceCubes(const LastStep: Integer);
  public
    procedure ScenePrepare;   
    procedure SceneStepForvard(const PrevStep, LastStep: Integer);
  end;

var
  f3DView: Tf3DView;

implementation

uses
  uMain, uServiceDM, uCameraDM;

{$R *.dfm}

{ Tf3DView }

procedure Tf3DView.btn3DViewCloseClick(Sender: TObject);
begin
  fMain.CheckBox3DView.Checked := False;
end;

procedure Tf3DView.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMain.CheckBox3DView.Checked := False;
end;

procedure Tf3DView.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  fServiceDM.GLCameraCamView.AdjustDistanceToTarget(Power(1.03, -WheelDelta/40));
end;

procedure Tf3DView.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  case Msg.CharCode of
    VK_ESCAPE: btn3DViewCloseClick(btn3DViewClose);
  end;
end;

procedure Tf3DView.GLViewerCamsFormDblClick(Sender: TObject);
begin
  with fServiceDM.GLCameraCamView do
    begin
      Position.X := 5;
      Position.Y := 5;
      Position.Z := 5;
      TargetObject.Position.X := 0;
      TargetObject.Position.Y := 0;
      TargetObject.Position.Z := 0;
    end;
end;

var
  FoldMouseX : integer;
  FoldMouseY : integer;

procedure Tf3DView.GLViewerCamsFormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with fServiceDM.GLCameraCamView do
    if ssLeft in Shift then
      MoveAroundTarget(FoldMouseY - Y, FoldMouseX - X)
    else
      if ssRight in Shift then
        begin
          MoveInEyeSpace(0, (FoldMouseX - X) / 10, (FoldMouseY - Y) / 10);
          MoveTargetInEyeSpace(0, (FoldMouseX - X) / 10, -(FoldMouseY - Y) / 10);
        end;
  FoldMouseX := X;
  FoldMouseY := Y;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Tf3DView.ClearScene;
var
  I: Integer;
begin
  with fServiceDM.GLScene1.Objects do
    for I := 0 to Length(FSceneLines) - 1 do
      begin
        Remove(FSceneLines[I], False);
        Remove(FSceneCubes[I], False);
        if Assigned(FSceneLines[I]) then
          FreeAndNil(FSceneLines[I]);
        if Assigned(FSceneCubes[I]) then
          FreeAndNil(FSceneCubes[I]);
      end;
  SetLength(FSceneLines, 0);
  SetLength(FSceneCubes, 0);
end;

procedure Tf3DView.SceneStepForvard(const PrevStep, LastStep: Integer);
var
  I, J: Integer;
begin
  ScenePlaceCubes(LastStep);
  for I := 0 to Length(FSceneLines) - 1 do
    for J := PrevStep to LastStep - 1 do
      with fCameraDM.CaptureFile.Coordinates[J] do
        FSceneLines[I].Nodes.AddNode(PointsMetric[I].X, PointsMetric[I].Y, PointsMetric[I].Z);
end;

procedure Tf3DView.ScenePlaceCubes(const LastStep: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(FSceneCubes) - 1 do
    with fServiceDM.MCFile.Coordinates[fCameraDM.CaptureFile.CoordinateCount - 1] do
      FSceneCubes[I].Position.SetPoint(PointsMetric[I].X, PointsMetric[I].Y, PointsMetric[I].Z);
end;

procedure Tf3DView.ScenePrepare;
var
  I: Integer;
begin
  ClearScene;
  with fCameraDM.CaptureFile do
    begin
      SetLength(FSceneLines, Options['PointCount'].AsInteger);
      SetLength(FSceneCubes, Options['PointCount'].AsInteger);
      for I := 0 to Options['PointCount'].AsInteger - 1 do
        begin
          FSceneLines[I] := TGLLines.Create(Self);
          FSceneLines[I].Division := 500;
          FSceneLines[I].LineWidth := 1;
          FSceneLines[I].NodesAspect := lnaInvisible;
          FSceneLines[I].NodeSize := 0.2;
          FSceneLines[I].Options := [loUseNodeColorForLines];
          with fServiceDM.MCFile.Options['LineColor'] do
            begin
              FSceneLines[I].NodeColor.Red := fServiceDM.HexToInt(Copy(AsString, 1, 2)) / 255;
              FSceneLines[I].NodeColor.Green := fServiceDM.HexToInt(Copy(AsString, 3, 2)) / 255;
              FSceneLines[I].NodeColor.Blue := fServiceDM.HexToInt(Copy(AsString, 5, 2)) / 255;
            end;
          FSceneLines[I].SplineMode := lsmLines;

          FSceneCubes[I] := TGLDummyCube.Create(Self);
          FSceneCubes[I].CubeSize := 0.2;
          FSceneCubes[I].VisibleAtRunTime := True;
          with fServiceDM.MCFile.Options['LineColor'] do
            begin
              FSceneCubes[I].EdgeColor.Red := fServiceDM.HexToInt(Copy(AsString, 1, 2)) / 255;
              FSceneCubes[I].EdgeColor.Green := fServiceDM.HexToInt(Copy(AsString, 3, 2)) / 255;
              FSceneCubes[I].EdgeColor.Blue := fServiceDM.HexToInt(Copy(AsString, 5, 2)) / 255;
            end;

          fServiceDM.GLScene1.Objects.AddChild(FSceneLines[I]);
          fServiceDM.GLScene1.Objects.AddChild(FSceneCubes[I]);
        end;
    end;
end;

end.
