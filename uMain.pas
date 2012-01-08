unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DSPack, DirectShow9, ComCtrls, ActnList, XPStyleActnCtrls, ActnMan, ExtCtrls, ImgList, GLScene,
  GLGeomObjects, GLObjects, GLGraph, LibXmlParser, LibXmlComps, XPMan, OleCtrls, SHDocVw, uConsole_1,
  GLWin32Viewer, VirtualTrees, Buttons, uGradient, StdStyleActnCtrls, GLMisc, GLCrossPlatform,
  uCameraDM, uServiceDM;

type
  TPlayBtnType = (btPlay, btStop, btPause, btStepBack, btStepForvard, btBack, btForvard);
  TControlButton = (cbZoomIn, cbZoomOut, cbRotateUp, cbRotateLeft, cbRotateRight, cbRotateDown, cbUpdate);

  TfMain = class(TForm)
    ActionManager1: TActionManager;
    acStartTest: TAction;
    acMouseControl: TAction;
    ac3DView: TAction;
    StBar: TStatusBar;
    PageMain: TPageControl;
    TabSheet1: TTabSheet;
    PageAnalize: TPageControl;
    TabSheet4: TTabSheet;
    TreeInfo: TVirtualStringTree;
    TabSheet5: TTabSheet;
    Tree3DTable: TVirtualStringTree;
    PanelAnalHead: T3DGradientPanel;
    Label4: TLabel;
    Label1: TLabel;
    Image1: TImage;
    ComboTestList: TComboBox;
    TabSheet2: TTabSheet;
    Panel4: T3DGradientPanel;
    Label3: TLabel;
    Image3: TImage;
    btnStartTest: TSpeedButton;
    ScrollBox1: TScrollBox;
    GroupBox1: TGroupBox;
    lblTrashhold1: TLabel;
    lblMinPointSize1: TLabel;
    lblMaxPointSize1: TLabel;
    btnPreview1: TSpeedButton;
    Label5: TLabel;
    ComboSource1: TComboBox;
    Treshhold1: TTrackBar;
    VideoWindow1: TVideoWindow;
    MinPointSize1: TTrackBar;
    MaxPointSize1: TTrackBar;
    Treshhold1Edit: TEdit;
    Treshhold1UpDown: TUpDown;
    MinPointSize1Edit: TEdit;
    MinPointSize1UpDown: TUpDown;
    MaxPointSize1Edit: TEdit;
    MaxPointSize1UpDown: TUpDown;
    CheckBoxDisableFilter1: TCheckBox;
    EditCameraDegree1: TEdit;
    GroupBox2: TGroupBox;
    lblTrashhold2: TLabel;
    lblMinPointSize2: TLabel;
    lblMaxPointSize2: TLabel;
    btnPreview2: TSpeedButton;
    Label6: TLabel;
    ComboSource2: TComboBox;
    Treshhold2: TTrackBar;
    VideoWindow2: TVideoWindow;
    MinPointSize2: TTrackBar;
    MaxPointSize2: TTrackBar;
    Treshhold2Edit: TEdit;
    Treshhold2UpDown: TUpDown;
    MinPointSize2UpDown: TUpDown;
    MaxPointSize2UpDown: TUpDown;
    MaxPointSize2Edit: TEdit;
    MinPointSize2Edit: TEdit;
    CheckBoxDisableFilter2: TCheckBox;
    EditCameraDegree2: TEdit;
    gbParams: TGroupBox;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label19: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label20: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    EditMass: TEdit;
    EditInterval: TEdit;
    EditCamRadius: TEdit;
    EditCamHeight: TEdit;
    EditTestName: TEdit;
    EditX0: TEdit;
    EditY0: TEdit;
    EditZ0: TEdit;
    CheckBoxSyncCamSettings: TCheckBox;
    CheckBoxMouseMove: TCheckBox;
    CheckBox3DView: TCheckBox;
    TabScripts: TTabSheet;
    GradientPanel1: T3DGradientPanel;
    Label18: TLabel;
    Image4: TImage;
    ScriptConsole1: TScriptConsole;
    TabSheet3: TTabSheet;
    Panel3: T3DGradientPanel;
    Label2: TLabel;
    Guide: TSpeedButton;
    About: TSpeedButton;
    Image5: TImage;
    PageHelp: TPageControl;
    TabHelp: TTabSheet;
    Browser: TWebBrowser;
    TabAbout: TTabSheet;
    GradientPanel2: T3DGradientPanel;
    Image2: TImage;
    LabelName: TLabel;
    LabelVersion: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    PlayTimer: TTimer;
    ComboMediaTypes1: TComboBox;
    ComboMediaTypes2: TComboBox;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    ComboPointCount: TComboBox;
    pnl3dView: TPanel;
    pnl3dViewControls: TPanel;
    GLViewerMain: TGLSceneViewer;
    btnPlay: TSpeedButton;
    btnPause: TSpeedButton;
    btnStop: TSpeedButton;
    btnStepForvard: TSpeedButton;
    btnStepBack: TSpeedButton;
    btnBack: TSpeedButton;
    btnForvard: TSpeedButton;
    trAnimation: TTrackBar;
    btnZoomIn: TSpeedButton;
    btnZoomOut: TSpeedButton;
    btnRotateLeft: TSpeedButton;
    btnRotateRight: TSpeedButton;
    btnRotateUp: TSpeedButton;
    btnRotateDown: TSpeedButton;
    CheckBoxAnimation: TCheckBox;
    CheckBoxShowCamera: TCheckBox;
    CheckBoxEnablePerspective: TCheckBox;
    CheckBoxShowGridAndAxes: TCheckBox;
    Splitter3DTable: TSplitter;
    Splitter2: TSplitter;
    cbTrajectory: TComboBox;
    cbNeedCountTrajectory: TCheckBox;
    btnTestDebug: TButton;
    btnSaveSettings: TButton;
    btnUpdate: TSpeedButton;
    CheckBoxGrid1: TCheckBox;
    CheckBoxGrid2: TCheckBox;
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnTestDebugClick(Sender: TObject);
    procedure EditTestNameChange(Sender: TObject);
    procedure cbNeedCountTrajectoryClick(Sender: TObject);
    procedure Tree3DTableClick(Sender: TObject);
    procedure CheckBoxShowGridAndAxesClick(Sender: TObject);
    procedure trAnimationChange(Sender: TObject);
    procedure CheckBoxEnablePerspectiveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TreeInfoBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TreeInfoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure TreeInfoGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure Tree3DTableGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure Tree3DTableGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure CheckBoxMouseMoveClick(Sender: TObject);
    procedure CheckBox3DViewClick(Sender: TObject);
    procedure CheckBoxSyncCamSettingsClick(Sender: TObject);
    procedure EditMassChange(Sender: TObject);
    procedure CheckEditKeyPress(Sender: TObject; var Key: Char);
    procedure acStartTestExecute(Sender: TObject);
    procedure btnStartTestClick(Sender: TObject);
    procedure FilterPropertyChange(Sender: TObject);
    procedure ComboMediaTypesChange(Sender: TObject);
    procedure ComboSourceChange(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure HelpPanelSwitch(Sender: TObject);
    procedure SceneControlButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlayTimerTimer(Sender: TObject);
    procedure CheckBoxAnimationClick(Sender: TObject);
    procedure CheckBoxShowCameraClick(Sender: TObject);
    procedure GLViewerMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLViewerMainClick(Sender: TObject);
    procedure GLViewerMainDblClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageMainChange(Sender: TObject);
    procedure ComboTestListChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboTestListDropDown(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
  protected
    procedure EnableCameraControls(const Index: String; const Enabled: Boolean);
  private
    FSceneLines: array of TGLLines;
    FSceneCubes: array of TGLDummyCube;
    FSceneAnimateLastStep: Integer;

    FSceneAnimatePaused: Boolean;

    FTrackBarChange: TNotifyEvent;

    function GetOptionCaption(const Name: String): String;

    procedure ClearScene;
    procedure CreateScene;
    procedure SceneShowCameras(Visible: Boolean; R: Real = -1; H: Real = -1);
    procedure SceneCenterCamera;
    procedure SceneCreatePath;
    procedure ScenePrepare;
    procedure SceneMakeTrajectoryList;
    procedure SceneMakeTable;
    procedure SceneMakeAnimateTable;

    procedure SceneAnimateBegin;
    procedure SceneAnimateStepBack(StepCount: Integer = 1);
    procedure SceneAnimateStepForvard(StepCount: Integer = 1);
    procedure SceneAnimateStepTo(const Step: Integer);
    procedure SceneAnimatePlaceCubes;
    procedure SceneAnimateClear;
    procedure SceneAnimateEnd;

    procedure SceneTableSelectByIndex(const Index: Integer);

    procedure SceneAnimateBeginUpdate;
    procedure SceneAnimateEndUpdate;

    procedure FillComboTestList;

    procedure FillCameraComboBoxes;
    procedure ChangeCamera(const Index: Integer);
    procedure ChangeMediaType(const Index: Integer);
    procedure StartPreview(const Index: Integer);
    procedure StopPreview(const Index: Integer);

    function StartTest: Boolean;
    function StopTest: Boolean;

    procedure TestMouseMove;
    procedure TestWatch3D;

    procedure ChangeStartButtonImage(var btn: TSpeedButton);
    procedure LoadHelp;
  public
    procedure ChangePreviewButtonImage(var btn: TSpeedButton);
    procedure LoadFile(const Name: String);
  end;

var
  fMain: TfMain;

implementation

uses
  uDebug, Math, uMCPoint, uParams, u3DView, uVTData, uMCCounter, uTestDebug;

{$R *.dfm}

// =====================================================================================================================
// Event handlers ------------------------------------------------------------------------------------------------------
// =====================================================================================================================

procedure TfMain.acStartTestExecute(Sender: TObject);
begin
  btnStartTest.Down := not btnStartTest.Down;
  btnStartTest.Click;
end;

procedure TfMain.trAnimationChange(Sender: TObject);
begin
  PlayTimer.Interval := fServiceDM.MCFile.Options['Interval'].AsInteger;
  PlayTimer.Enabled := False;
  FSceneAnimatePaused := True;
  SceneAnimateStepTo(TTrackBar(Sender).Position);
end;

procedure TfMain.btnPlayClick(Sender: TObject);
const
  MOVE_COUNT = 15;
begin
  PlayTimer.Interval := fServiceDM.MCFile.Options['Interval'].AsInteger;
  case TPlayBtnType(TSpeedButton(Sender).Tag) of
    btPlay:
      begin
        if not FSceneAnimatePaused then
          begin
            SceneAnimateStepTo(0);
            SceneAnimateBegin;
            FSceneAnimatePaused := False;
          end
        else
          PlayTimer.Enabled := True;
      end;
    btPause:
      begin
        PlayTimer.Enabled := False;
        FSceneAnimatePaused := True;
      end;
    btStop:
      begin
        SceneAnimateEnd;
        FSceneAnimatePaused := False;
      end;
    btStepBack:
      begin
        PlayTimer.Enabled := False;
        SceneAnimateStepBack;
        FSceneAnimatePaused := True;
      end;
    btStepForvard:
      begin
        PlayTimer.Enabled := False;
        SceneAnimateStepForvard;
        FSceneAnimatePaused := True;
      end;
    btBack:
      begin
        PlayTimer.Enabled := False;
        SceneAnimateStepBack(MOVE_COUNT);
        FSceneAnimatePaused := True;
      end;
    btForvard:
      begin
        PlayTimer.Enabled := False;
        SceneAnimateStepForvard(MOVE_COUNT);
        FSceneAnimatePaused := True;
      end;
  end;
end;

procedure TfMain.btnPreviewClick(Sender: TObject);
begin
  if TSpeedButton(Sender).Down then
    StartPreview(TSpeedButton(Sender).Tag)
  else
    StopPreview(TSpeedButton(Sender).Tag);
end;

procedure TfMain.btnSaveSettingsClick(Sender: TObject);
begin
  Params.SaveParams;
end;

procedure TfMain.btnStartTestClick(Sender: TObject);
begin
  try
    if TSpeedButton(Sender).Down then
      begin
        if not StartTest then
          TSpeedButton(Sender).Down := False;
      end
    else
      begin
        if not StopTest then
          TSpeedButton(Sender).Down := True;
      end;
  finally
    ChangeStartButtonImage(TSpeedButton(Sender));
  end;
end;

procedure TfMain.cbNeedCountTrajectoryClick(Sender: TObject);
begin
  Tree3DTable.Visible := TCheckBox(Sender).Checked;
  Splitter3DTable.Visible := TCheckBox(Sender).Checked;
  if TCheckBox(Sender).Checked then
    SceneMakeTable;
end;

procedure TfMain.SceneControlButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with fServiceDM do
    case TControlButton(TComponent(Sender).Tag) of
      cbZoomIn:
        GLCameraMain.AdjustDistanceToTarget(0.98);
      cbZoomOut:
        GLCameraMain.AdjustDistanceToTarget(1.02);
      cbRotateUp:
        GLCameraMain.MoveAroundTarget(2, 0);
      cbRotateLeft:
        GLCameraMain.MoveAroundTarget(0, 2);
      cbRotateRight:
        GLCameraMain.MoveAroundTarget(0, -2);
      cbRotateDown:
        GLCameraMain.MoveAroundTarget(-2, 0);
      cbUpdate:
        SceneAnimateEnd;
    end;
end;

procedure TfMain.CheckBox3DViewClick(Sender: TObject);
begin
  TestWatch3D;
end;

procedure TfMain.CheckBoxAnimationClick(Sender: TObject);
begin
  fServiceDM.AnimateTimer.Enabled := TCheckBox(Sender).Checked;
  Params['Animation'].AsBoolean := CheckBoxAnimation.Checked;
end;

procedure TfMain.CheckBoxEnablePerspectiveClick(Sender: TObject);
begin
  Params['EnablePerspective'].AsBoolean := TCheckBox(Sender).Checked;
end;

procedure TfMain.CheckBoxMouseMoveClick(Sender: TObject);
begin
  TestMouseMove;
end;

procedure TfMain.CheckBoxShowCameraClick(Sender: TObject);
begin
  SceneShowCameras(TCheckBox(Sender).Checked);
  Params['ShowCamera'].AsBoolean := TCheckBox(Sender).Checked;
end;

procedure TfMain.CheckBoxShowGridAndAxesClick(Sender: TObject);
begin
  fServiceDM.GLGrid.Visible := TCheckBox(Sender).Checked;
end;

procedure TfMain.CheckBoxSyncCamSettingsClick(Sender: TObject);
begin
  Params['SyncCamSettings'].AsBoolean := TCheckBox(Sender).Checked;
end;

procedure TfMain.CheckEditKeyPress(Sender: TObject; var Key: Char);
begin
  case TEdit(Sender).Tag of
    0, 3..9:
      if not fServiceDM.IsFloat(Key, TEdit(Sender).Text) then
        Key := #0;
    1:
      if not fServiceDM.IsColor(Key) then
        Key := #0;
    2:
      if not fServiceDM.IsDigit(Key) then
        Key := #0;
  end;
end;

procedure TfMain.ComboMediaTypesChange(Sender: TObject);
begin
  ChangeMediaType(TComboBox(Sender).Tag);
end;

procedure TfMain.ComboSourceChange(Sender: TObject);
begin
  ChangeCamera(TComboBox(Sender).Tag);
  EnableCameraControls(IntToStr(TComboBox(Sender).Tag), TComboBox(Sender).ItemIndex > 0);
end;

procedure TfMain.EditMassChange(Sender: TObject);
var
  ParName: String;
begin
  ParName := Copy(TEdit(Sender).Name, 5, Length(TEdit(Sender).Name));
  Params[ParName].AsString := TEdit(Sender).Text;
end;

procedure TfMain.EditTestNameChange(Sender: TObject);
begin
  Caption := PROGRAM_NAME + ' v' + VERSION + ' [ ' + EditTestName.Text + ' ]';
end;

procedure TfMain.EnableCameraControls(const Index: String; const Enabled: Boolean);
var
  Panel: TGroupBox;
  I: Integer;
begin
  Panel := TGroupBox(FindComponent('GroupBox' + Index));
  if Panel <> nil then
    for I := 0 to ComponentCount - 1 do
      if not ((Components[I] is TVideoWindow) or (Components[I].Name = ('ComboSource' + Index))) and (TControl(Components[I]).Parent = Panel) then
        TControl(Components[I]).Enabled := Enabled;
end;

procedure TfMain.ComboTestListChange(Sender: TObject);
begin
  pnl3dViewControls.Enabled := TComboBox(Sender).ItemIndex <> -1;
  if pnl3dViewControls.Enabled then
    SceneAnimateEnd;
end;

procedure TfMain.ComboTestListDropDown(Sender: TObject);
begin
  FillComboTestList;
end;

procedure TfMain.FillComboTestList;
begin
  with fServiceDM.GetFileList(Params['TestsDir'].AsString, '*.xml') do
    begin
      ComboTestList.Items.Text := Text;
      Free;
    end;
end;

procedure TfMain.FilterPropertyChange(Sender: TObject);
type
  TSenderType = (stNone, stCheckBox, stTrackBar, stComboBox, stEdit);

  function SenderType: TSenderType;
  begin
    Result := stNone;
    if Sender is TComboBox then
      Result := stComboBox
    else if Sender is TTrackBar then
      Result := stTrackBar
    else if Sender is TCheckBox then
      Result := stCheckBox
    else if Sender is TEdit then
      Result := stEdit;
  end;

  // Odd index is for second camera
begin
  with CameraManager do
    if HasCameras then
      begin
        case SenderType of
          stCheckBox:
            case TCheckBox(Sender).Tag of
              0:
                begin
                  FirstCamera.Filter.Active := not TCheckBox(Sender).Checked;
                  Params['DisableFilter1'].AsBoolean := TCheckBox(Sender).Checked;
                end;
              1:
                begin
                  SecondCamera.Filter.Active := not TCheckBox(Sender).Checked;
                  Params['DisableFilter2'].AsBoolean := TCheckBox(Sender).Checked;
                end;
              2:
                begin
                  FirstCamera.Filter.ShowGrid := TCheckBox(Sender).Checked;
                  Params['ShowGrid1'].AsBoolean := TCheckBox(Sender).Checked;
                end;
              3:
                begin
                  SecondCamera.Filter.ShowGrid := TCheckBox(Sender).Checked;
                  Params['ShowGrid2'].AsBoolean := TCheckBox(Sender).Checked;
                end;
            end;
          stTrackBar:
            case TTrackBar(Sender).Tag of
              0:
                begin
                  FirstCamera.Filter.Treshhold := TTrackBar(Sender).Position;
                  Treshhold1Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> Treshhold2.Position then
                      Treshhold2.Position := TTrackBar(Sender).Position;
                  Params['Treshhold1'].AsInteger := TTrackBar(Sender).Position;
                end;
              1:
                begin
                  SecondCamera.Filter.Treshhold := TTrackBar(Sender).Position;
                  Treshhold2Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> Treshhold1.Position then
                      Treshhold1.Position := TTrackBar(Sender).Position;
                  Params['Treshhold2'].AsInteger := TTrackBar(Sender).Position;
                end;
              2:
                begin
                  FirstCamera.Filter.MinPointSize := TTrackBar(Sender).Position;
                  MinPointSize1Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> MinPointSize2.Position then
                      MinPointSize2.Position := TTrackBar(Sender).Position;
                  if TTrackBar(Sender).Position >= MaxPointSize1.Position then
                    MaxPointSize1.Position := TTrackBar(Sender).Position + 1;
                  Params['MinPointSize1'].AsInteger := TTrackBar(Sender).Position;
                end;
              3:
                begin
                  SecondCamera.Filter.MinPointSize := TTrackBar(Sender).Position;
                  MinPointSize2Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> MinPointSize1.Position then
                      MinPointSize1.Position := TTrackBar(Sender).Position;
                  if TTrackBar(Sender).Position >= MaxPointSize2.Position then
                    MaxPointSize2.Position := TTrackBar(Sender).Position + 1;
                  Params['MinPointSize2'].AsInteger := TTrackBar(Sender).Position;
                end;
              4:
                begin
                  FirstCamera.Filter.MaxPointSize := TTrackBar(Sender).Position;
                  MaxPointSize1Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> MaxPointSize2.Position then
                      MaxPointSize2.Position := TTrackBar(Sender).Position;
                  if TTrackBar(Sender).Position <= MinPointSize1.Position then
                    MinPointSize1.Position := TTrackBar(Sender).Position - 1;
                  Params['MaxPointSize1'].AsInteger := TTrackBar(Sender).Position;
                end;
              5:
                begin
                  SecondCamera.Filter.MaxPointSize := TTrackBar(Sender).Position;
                  MaxPointSize2Edit.Text := IntToStr(TTrackBar(Sender).Position);
                  if CheckBoxSyncCamSettings.Checked then
                    if TTrackBar(Sender).Position <> MaxPointSize1.Position then
                      MaxPointSize1.Position := TTrackBar(Sender).Position;
                  if TTrackBar(Sender).Position <= MinPointSize2.Position then
                    MinPointSize2.Position := TTrackBar(Sender).Position - 1;
                  Params['MaxPointSize2'].AsInteger := TTrackBar(Sender).Position;
                end;
            end;
          stComboBox:
            begin
              FirstCamera.Filter.PointCount := StrToInt(TComboBox(Sender).Items[TComboBox(Sender).ItemIndex]);
              SecondCamera.Filter.PointCount := StrToInt(TComboBox(Sender).Items[TComboBox(Sender).ItemIndex]);
              Params['PointCount'].AsInteger := TComboBox(Sender).ItemIndex + 1;
            end;
          stEdit:
            case TEdit(Sender).Tag of
              0: Treshhold1.Position := StrToInt(TEdit(Sender).Text);
              1: Treshhold2.Position := StrToInt(TEdit(Sender).Text);
              2: MinPointSize1.Position := StrToInt(TEdit(Sender).Text);
              3: MinPointSize2.Position := StrToInt(TEdit(Sender).Text);
              4: MaxPointSize1.Position := StrToInt(TEdit(Sender).Text);
              5: MaxPointSize2.Position := StrToInt(TEdit(Sender).Text);
            end;
        end;
      end;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if CameraManager.IsCapturing then
    begin
      CameraManager.StopCapture;
      // Give filter a chance to free itself
      Sleep(3000);
    end
  else
    begin
      if CameraManager.FirstCamera.IsCapturing then
        CameraManager.FirstCamera.StopCapture;
      if CameraManager.SecondCamera.IsCapturing then
        CameraManager.SecondCamera.StopCapture;
      // Give filter a chance to free itself
      Sleep(3000);
    end;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TPageControl then
      TPageControl(Components[I]).ActivePageIndex := 0
    else if Components[I] is TSpeedButton then
      TSpeedButton(Components[I]).Caption := StringReplace(TSpeedButton(Components[I]).Caption, '_', #13#10, [rfReplaceAll]);

  LabelName.Caption := PROGRAM_NAME + ' v.' + VERSION;
  LabelVersion.Caption := LabelVersion.Caption + ' ' + VERSION;

  FillComboTestList;
  if ComboTestList.Items.Count > 0 then
    begin
      ComboTestList.ItemIndex := 0;
      CreateScene;
      SceneCenterCamera;
    end;
  ScriptConsole1.SourcePath := ExpandFileName('.\Source\');

  GLViewerMain.Buffer.BackgroundColor := fServiceDM.HexToInt(Params['GLBackground'].AsString);
  with fServiceDM.HexToGlColor(Params['GlGridColor'].AsString) do
    begin
      fServiceDM.GLGrid.LineColor.Red := Red;
      fServiceDM.GLGrid.LineColor.Green := Green;
      fServiceDM.GLGrid.LineColor.Blue := Blue;
      Free;
    end;

  pnl3dView.Width := Screen.Width - 240;
end;

procedure TfMain.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  if GLViewerMain.Focused then
    with fServiceDM do
      case PageMain.ActivePageIndex of
        0: GLCameraMain.AdjustDistanceToTarget(Power(1.02, -WheelDelta/120));
        1: GLCameraCamView.AdjustDistanceToTarget(Power(1.02, -WheelDelta/120));
      end;
end;

procedure TfMain.GLViewerMainClick(Sender: TObject);
begin
  TGLSceneViewer(Sender).SetFocus;
end;

procedure TfMain.GLViewerMainDblClick(Sender: TObject);
begin
  SceneCenterCamera;
end;

var
  FoldMouseX, FoldMouseY: Integer;
procedure TfMain.GLViewerMainMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  with fServiceDM do
    if ssLeft in Shift then
      case PageMain.ActivePageIndex of
        0: GLCameraMain.MoveAroundTarget(FoldMouseY - Y, FoldMouseX - X);
        1: GLCameraCamView.MoveAroundTarget(FoldMouseY - Y, FoldMouseX - X);
      end
    else
      if ssRight in Shift then
        case PageMain.ActivePageIndex of
          0:
            begin
              GLCameraMain.MoveInEyeSpace(0, (FoldMouseX - X) / 10, (FoldMouseY - Y) / 10);
              GLCameraMain.MoveTargetInEyeSpace(0, (FoldMouseX - X) / 10, -(FoldMouseY - Y) / 10);
            end;
          1:
            begin
              GLCameraCamView.MoveInEyeSpace(0, (FoldMouseX - X) / 10, (FoldMouseY - Y) / 10);
              GLCameraCamView.MoveTargetInEyeSpace(0, (FoldMouseX - X) / 10, -(FoldMouseY - Y) / 10);
            end;
        end;
  FoldMouseX := X;
  FoldMouseY := Y;
end;

procedure TfMain.HelpPanelSwitch(Sender: TObject);
begin
  PageHelp.ActivePageIndex := TSpeedButton(Sender).Tag;
end;

procedure TfMain.PageMainChange(Sender: TObject);
begin
  if TPageControl(Sender).ActivePageIndex <> 1 then
    f3DView.Close;

  case TPageControl(Sender).ActivePageIndex of
    0:
      begin
        Caption := PROGRAM_NAME + ' v' + VERSION + ' [ ' + ComboTestList.Items[ComboTestList.ItemIndex] + ' ]';
        ComboTestListChange(ComboTestList);
      end;
    1:
      begin
        FillCameraComboBoxes;
        EditTestName.Text := fServiceDM.GetFileNameByMask;
        ClearScene;
        SceneShowCameras(True, StrToFloat(EditCamRadius.Text), StrToFloat(EditCamHeight.Text));
      end;
    2: Caption := PROGRAM_NAME + ' v' + VERSION + ' [ Скрипт-консоль ]';
    3: LoadHelp;
  end;
end;

procedure TfMain.PlayTimerTimer(Sender: TObject);
var
  T: Cardinal;
begin
  T := fServiceDM.MCFile.Coordinates[FSceneAnimateLastStep].Time;
  SceneAnimateStepForvard;
  if T <= 0 then
    T := 10;
  TTimer(Sender).Interval := T;
end;

// Event handlers end
// =====================================================================================================================
// 3D Scene Functions --------------------------------------------------------------------------------------------------
// =====================================================================================================================

procedure TfMain.ClearScene;
begin
  with fServiceDM do
    begin
      PlayTimer.Enabled := False;
      SceneAnimateClear;
    end;
end;

procedure TfMain.CreateScene;
begin
  Caption := PROGRAM_NAME + ' v' + VERSION + ' [ ' + ComboTestList.Items[ComboTestList.Items.Count - 1] + ' ]';
  ClearScene;
  LoadFile(Params['TestsDir'].AsString + ComboTestList.Items[ComboTestList.ItemIndex]);
  SceneShowCameras(CheckBoxShowCamera.Checked, fServiceDM.MCFile.Options['CameraRadius'].AsFloat, fServiceDM.MCFile.Options['CameraHeight'].AsFloat);
//  SceneCenterCamera;
  SceneCreatePath;
  SceneMakeAnimateTable;
  SceneMakeTrajectoryList;
  if cbNeedCountTrajectory.Checked then
    SceneMakeTable;
  FSceneAnimatePaused := False;
end;

procedure TfMain.SceneAnimateBegin;
begin
  ScenePrepare;
  PlayTimer.Interval := fServiceDM.MCFile.Coordinates[FSceneAnimateLastStep].Time;
  PlayTimer.Enabled := True;
end;

procedure TfMain.SceneAnimateClear;
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

procedure TfMain.SceneAnimateEnd;
begin
  CreateScene;
end;

procedure TfMain.SceneAnimateBeginUpdate;
begin
  FTrackBarChange := trAnimation.OnChange;
  trAnimation.OnChange := nil;
end;

procedure TfMain.SceneAnimateEndUpdate;
begin
  trAnimation.OnChange := FTrackBarChange;
end;

procedure TfMain.SceneAnimatePlaceCubes;
var
  I: Integer;
begin
  for I := 0 to Length(FSceneCubes) - 1 do
    with fServiceDM.MCFile.Coordinates[FSceneAnimateLastStep] do
      FSceneCubes[I].Position.SetPoint(PointsMetric[I].X, PointsMetric[I].Y, PointsMetric[I].Z);
end;

procedure TfMain.SceneAnimateStepBack(StepCount: Integer);
var
  I, J: Integer;
begin
  SceneAnimateBeginUpdate;

  try
    PlayTimer.Enabled := False;

    if FSceneAnimateLastStep - StepCount + 1 < 0 then
      StepCount := FSceneAnimateLastStep;
    if FSceneAnimateLastStep = 0 then
      Exit;

    trAnimation.Position := FSceneAnimateLastStep - StepCount;

    for I := 0 to StepCount - 1 do
      begin
        for J := 0 to Length(FSceneLines) - 1 do
          if FSceneLines[J].Nodes.Count > FSceneAnimateLastStep then
            FSceneLines[J].Nodes.Delete(FSceneAnimateLastStep);
        Dec(FSceneAnimateLastStep);
      end;
    SceneAnimatePlaceCubes;
    SceneTableSelectByIndex(FSceneAnimateLastStep);    
  finally
    SceneAnimateEndUpdate
  end;
end;

procedure TfMain.SceneAnimateStepForvard(StepCount: Integer);
var
  I, J: Integer;
begin
  SceneAnimateBeginUpdate;

  try
    if FSceneAnimateLastStep >= fServiceDM.MCFile.CoordinateCount - 1 then
      begin
        SceneAnimateEnd;
        Exit;
      end;

    if (StepCount + FSceneAnimateLastStep) >= fServiceDM.MCFile.CoordinateCount then
      StepCount := fServiceDM.MCFile.CoordinateCount - FSceneAnimateLastStep - 1;

    trAnimation.Position := FSceneAnimateLastStep + StepCount;

    SceneAnimatePlaceCubes;
    for I := 0 to StepCount - 1 do
      begin
        for J := 0 to Length(FSceneLines) - 1 do
          with fServiceDM.MCFile.Coordinates[FSceneAnimateLastStep] do
            FSceneLines[J].Nodes.AddNode(PointsMetric[J].X, PointsMetric[J].Y, PointsMetric[J].Z);
        Inc(FSceneAnimateLastStep);
      end;

    SceneTableSelectByIndex(FSceneAnimateLastStep);
  finally
    SceneAnimateEndUpdate;
  end;
end;

procedure TfMain.SceneAnimateStepTo(const Step: Integer);
var
  StepDelta: Integer;
begin
  StepDelta := Step - FSceneAnimateLastStep;
  if StepDelta > 0 then
    SceneAnimateStepForvard(StepDelta)
  else if StepDelta < 0 then
    SceneAnimateStepBack(Abs(StepDelta));
end;

procedure TfMain.SceneCenterCamera;
var
  Cam: TGLCamera;
begin
  Cam := nil;
  with fServiceDM do
    case PageMain.ActivePageIndex of
      0: Cam := GLCameraMain;
      1: Cam := GLCameraCamView;
    end;
  Cam.Position.X := -1.2;
  Cam.Position.Y := 1.2;
  Cam.Position.Z := 1.2;
  Cam.TargetObject.Position.X := 0;
  Cam.TargetObject.Position.Y := fServiceDM.MCFile.Options['CameraHeight'].AsFloat / 1000;
  Cam.TargetObject.Position.Z := 0;
end;

procedure TfMain.SceneCreatePath;
var
  I, J: Integer;
  Pt: T3DPoint;
begin
  ScenePrepare;

  with fServiceDM.MCFile do
    begin
    for I := 0 to Options['PointCount'].AsInteger - 1 do
      begin
        for J := 0 to CoordinateCount - 1 do
          begin
            Pt := Coordinates[J].PointsMetric[I];
            FSceneLines[I].AddNode(Pt.X, Pt.Y, Pt.Z);
          end;
        FSceneCubes[I].Position.X := Pt.X;
        FSceneCubes[I].Position.Y := Pt.Y;
        FSceneCubes[I].Position.Z := Pt.Z;
      end;
    end;

  FSceneAnimateLastStep := fServiceDM.MCFile.CoordinateCount - 1;
  trAnimation.Position := FSceneAnimateLastStep;
end;

function TfMain.GetOptionCaption(const Name: String): String;
begin
  if Name = 'Mass'              then Result := 'Масса' else
  if Name = 'Interval'          then Result := 'Кадровая частота' else
  if Name = 'CameraRadius'      then Result := 'Расстояние до камеры' else
  if Name = 'PointCount'        then Result := 'Количество траекторий' else
  if Name = 'CameraHeight'      then Result := 'Высота камеры' else
  if Name = 'TestName'          then Result := 'Название' else
  if Name = 'X0'                then Result := 'X0' else
  if Name = 'Y0'                then Result := 'Y0' else
  if Name = 'Z0'                then Result := 'Z0' else
  if Name = 'Cam1Degree'        then Result := 'Угол кам. №1' else
  if Name = 'Cam2Degree'        then Result := 'Угол кам. №2' else
  if Name = 'Cam1ResX'          then Result := 'Кам. №1 ширина' else
  if Name = 'Cam1ResY'          then Result := 'Кам. №1 высота' else
  if Name = 'Cam2ResX'          then Result := 'Кам. №2 ширина' else
  if Name = 'Cam2ResY'          then Result := 'Кам. №2 высота' else
  if Name = 'EnablePerspective' then Result := 'С перспективой'
  else Result := Name;
end;

procedure TfMain.SceneMakeAnimateTable;
var
  Data: PSceneInfoData;
  I: Integer;
begin
  TreeInfo.Clear;
  with fServiceDM.MCFile, TreeInfo do
    begin
      for I := 0 to OptionCount - 1 do
        begin
          if GetOptionByIndex(I).Name = 'TestName' then
            Continue;

          Data := GetNodeData(AddChild(nil));
          Data.Caption := GetOptionCaption(GetOptionByIndex(I).Name);
          Data.Value := GetOptionByIndex(I).AsString;
        end;
      Data := GetNodeData(AddChild(nil));
      Data.Caption := 'Количество точек';
      Data.Value := IntToStr(CoordinateCount);

      FullExpand;
    end;
end;

procedure TfMain.SceneMakeTable;

  function AllowedLength(const S: TCountParameterTypes): Integer;
  var
    I: TCountParameterType;
  begin
    Result := 0;
    for I := Low(TCountParameterType) to High(TCountParameterType) do
      if I in S then
        Inc(Result);
  end;

const
  AllowedTypes: TCountParameterTypes = [cpaDeltaTime, cpaX, cpaY, cpaZ, cpaPathMod, cpaSpeedMod, cpaAccelMod, cpaPulseMod, cpaForceMod];

var
  I, J: Integer;
  Typ: TCountParameterType;
  Data: PSceneTableData;
  TrajectoryIndex: Integer;
  CCount: Integer;
begin
  Tree3DTable.Clear;
  Tree3DTable.BeginUpdate;

  try
    with Tree3DTable.Header do
      begin
        Columns.Clear;
        with Columns.Add do
          begin
            Text := '№ точки';
            Width := 50;
          end;

        with fServiceDM.MCCounter.GetNameWrapper(1) do
          for I := 0 to Count - 1 do
            if TCountParameterType(I) in AllowedTypes then
              with Columns.Add do
                begin
                  Text := Name[I];
                  Width := Length(Name[I]) * 8;
                  if Width > 150 then
                    Width := 150;
                end;
      end;

    TrajectoryIndex := cbTrajectory.ItemIndex;
    if TrajectoryIndex = -1 then
      Exit;

    with fServiceDM.MCFile, Tree3DTable do
      begin
        CCount := AllowedLength(AllowedTypes);
        for I := 0 to CoordinateCount - 1 do
          begin
            Data := GetNodeData(AddChild(nil));
            Data.Caption := 'Координата';
            Data.Value := IntToStr(I + 1);
            SetLength(Data.PointChars, CCount);
            J := 0;
            for Typ := Low(TCountParameterType) to High(TCountParameterType) do
              if Typ in AllowedTypes then
                begin
                  Data.PointChars[J] := FloatToStr(RoundTo(fServiceDM.MCCounter.Value[Integer(Typ), I, TrajectoryIndex], -3));
                  Inc(J);
                end;
          end;
      end;
  finally
    Tree3DTable.FullExpand;
    Tree3DTable.EndUpdate;

    SceneTableSelectByIndex(fServiceDM.MCFile.CoordinateCount - 1);
  end;
end;

procedure TfMain.SceneMakeTrajectoryList;
var
  I: Integer;
begin
  with cbTrajectory do
    try
      Clear;
      for I := 1 to fServiceDM.MCFile.Options['PointCount'].AsInteger do
        Items.Add('Траектория №' + IntToStr(I));
    finally
      ItemIndex := Integer(Items.Count > 0) - 1;
      Tree3DTable.Clear;
    end;
end;

procedure TfMain.ScenePrepare;
var
  I: Integer;
begin
  ClearScene;
  with fServiceDM.MCFile do
    begin
      trAnimation.Max := CoordinateCount - 1;

      SetLength(FSceneLines, Options['PointCount'].AsInteger);
      SetLength(FSceneCubes, Options['PointCount'].AsInteger);
      for I := 0 to Options['PointCount'].AsInteger - 1 do
        begin
          FSceneLines[I] := TGLLines.Create(Self);
          FSceneLines[I].LineWidth := Params['GLLineWidth'].AsFloat;
          if FSceneLines[I].LineWidth = 0 then
            FSceneLines[I].LineWidth := 1;
          FSceneLines[I].Division := 500;
          FSceneLines[I].NodesAspect := lnaInvisible;
          FSceneLines[I].NodeSize := 0.05;
          FSceneLines[I].Options := [loUseNodeColorForLines];
          with Params['GLLineColor'] do
            begin
              FSceneLines[I].NodeColor.Red := fServiceDM.HexToInt(Copy(AsString, 2, 2)) / 255;
              FSceneLines[I].NodeColor.Green := fServiceDM.HexToInt(Copy(AsString, 4, 2)) / 255;
              FSceneLines[I].NodeColor.Blue := fServiceDM.HexToInt(Copy(AsString, 6, 2)) / 255;
            end;
          FSceneLines[I].SplineMode := lsmLines;

          FSceneCubes[I] := TGLDummyCube.Create(Self);
          FSceneCubes[I].CubeSize := 0.05;
          FSceneCubes[I].VisibleAtRunTime := True;
          with Params['GLLineColor'] do
            begin
              FSceneCubes[I].EdgeColor.Red := fServiceDM.HexToInt(Copy(AsString, 2, 2)) / 255;
              FSceneCubes[I].EdgeColor.Green := fServiceDM.HexToInt(Copy(AsString, 4, 2)) / 255;
              FSceneCubes[I].EdgeColor.Blue := fServiceDM.HexToInt(Copy(AsString, 6, 2)) / 255;
            end;

          fServiceDM.GLScene1.Objects.AddChild(FSceneLines[I]);
          fServiceDM.GLScene1.Objects.AddChild(FSceneCubes[I]);
        end;
    end;
  FSceneAnimateLastStep := 0;
end;

procedure TfMain.SceneShowCameras(Visible: Boolean; R: Real = -1; H: Real = -1);
var
  I: Integer;
begin
  if R < 0 then
    R := fServiceDM.MCFile.Options['CameraRadius'].AsFloat;
  R := R / 1000;
  if H < 0 then
    H := fServiceDM.MCFile.Options['CameraHeight'].AsFloat;
  H := H / 1000;

  with fServiceDM do
    begin
      for I := 0 to GLCam1.Count - 1 do
        begin
          GLCam1.Children[i].Visible := Visible;
          GLCam2.Children[i].Visible := Visible;
        end;

      for I := 0 to GLTripod1.Count - 1 do
        begin
          GLTripod1.Children[i].Visible := Visible;
          GLTripod2.Children[i].Visible := Visible;
        end;

      GLCam1.Position.X := 0;
      GLCam1.Position.Y := H;
      GLCam1.Position.Z := R;

      GLCam2.Position.X := -R;
      GLCam2.Position.Y := H;
      GLCam2.Position.Z := 0;

      GLTripod1.Position.X := 0;
      GLTripod1.Position.Y := H / 2;
      GLTripod1.Position.Z := R;

      GLTripod2.Position.X := -R;
      GLTripod2.Position.Y := H / 2;
      GLTripod2.Position.Z := 0;

      if H >= 1 then
        begin
          GLTripod1.Scale.X := 1;
          GLTripod1.Scale.Y := H;
          GLTripod1.Scale.Z := 1;

          GLTripod2.Scale.X := 1;
          GLTripod2.Scale.Y := H;
          GLTripod2.Scale.Z := 1;
        end
      else
        begin
          GLTripod1.Scale.X := H;
          GLTripod1.Scale.Y := H;
          GLTripod1.Scale.Z := H;

          GLTripod2.Scale.X := H;
          GLTripod2.Scale.Y := H;
          GLTripod2.Scale.Z := H;
        end;

      GLGrid.XSamplingScale.Min := -R;
      GLGrid.XSamplingScale.Max := R;
      GLGrid.ZSamplingScale.Min := -R;
      GLGrid.ZSamplingScale.Max := R;
    end;
end;

procedure TfMain.SceneTableSelectByIndex(const Index: Integer);
var
  I: Integer;
  Node, SelectedNode: PVirtualNode;
begin
  if not cbNeedCountTrajectory.Checked then
    Exit;

  I := 0;
  with Tree3DTable do
    begin
      BeginUpdate;
      try
        Node := GetFirst;
        while Node <> nil do
          begin
            Selected[Node] := False;
            if Index = I then
              begin
                Selected[Node] := True;
                SelectedNode := Node;
              end;
            Node := GetNext(Node);
            Inc(I);
          end;
      finally
        EndUpdate;
        ScrollIntoView(SelectedNode, True);
      end;
    end;
end;

procedure TfMain.LoadFile(const Name: String);
begin
  fServiceDM.MCFile.LoadFile(Name);
end;

// 3D Scene end
// =====================================================================================================================
// Camera functions ----------------------------------------------------------------------------------------------------
// =====================================================================================================================

procedure TfMain.FillCameraComboBoxes;
begin
  with CameraManager.EnumCameras do
    begin
      ComboSource1.Items.Text := Text;
      ComboSource2.Items.Text := Text;
      Free;
    end;
  ComboSource1.ItemIndex := 0;
  ComboSource2.ItemIndex := 0;
  if ComboSource1.Items.Count > 1 then
    begin
      ComboSource1.ItemIndex := 1;
      ChangeCamera(1);
      EnableCameraControls('1', True);
    end;
  if ComboSource2.Items.Count > 2 then
    begin
      ComboSource2.ItemIndex := 2;
      ChangeCamera(2);
      EnableCameraControls('2', True);
    end;
end;

procedure TfMain.ChangeCamera(const Index: Integer);
begin
  case Index of
    1:
      begin
        CameraManager.FirstCamera.CameraIndex := ComboSource1.ItemIndex - 1;
        if ComboSource1.ItemIndex > 0 then
          begin
            with CameraManager.FirstCamera.EnumMediaTypes do
              begin
                ComboMediaTypes1.Items.Text := Text;
                Free;
              end;
            ComboMediaTypes1.ItemIndex := 0;
          end;
      end;
    2:
      begin
        CameraManager.SecondCamera.CameraIndex := ComboSource2.ItemIndex - 1;
        if ComboSource2.ItemIndex > 0 then
          begin
  
            with CameraManager.SecondCamera.EnumMediaTypes do
              begin
                ComboMediaTypes2.Items.Text := Text;
                Free;
              end;
            ComboMediaTypes2.ItemIndex := 0;
          end;
      end;
  end;
  ChangeMediaType(Index);
end;

procedure TfMain.ChangeMediaType(const Index: Integer);
var
  cp: TCapturePack;
  cb: TComboBox;
  IsCapturing: Boolean;
begin
  IsCapturing := False;
  cp := nil;
  cb := nil;
  case Index of
    1:
      begin
        cp := CameraManager.FirstCamera;
        cb := ComboMediaTypes1;
        IsCapturing := btnPreview1.Down;
      end;
    2:
      begin
        cp := CameraManager.SecondCamera;
        cb := ComboMediaTypes2;
        IsCapturing := btnPreview2.Down;
      end;
  end;
  if IsCapturing then
    StopPreview(Index);
  cp.MediaTypeIndex := cb.ItemIndex;
  if IsCapturing then
    StartPreview(Index);
end;

procedure TfMain.StartPreview(const Index: Integer);
begin
  case Index of
    1: CameraManager.FirstCamera.StartCapture;
    2: CameraManager.SecondCamera.StartCapture;
  end;
end;

function TfMain.StartTest: Boolean;
begin
  Result := CameraManager.StartCapture;
end;

procedure TfMain.StopPreview(const Index: Integer);
begin
  case Index of
    1: CameraManager.FirstCamera.StopCapture;
    2: CameraManager.SecondCamera.StopCapture;
  end;
end;

function TfMain.StopTest: Boolean;
begin
  Result := CameraManager.StopCapture;
end;

procedure TfMain.TestMouseMove;
begin

end;

procedure TfMain.TestWatch3D;
begin
  if CheckBox3DView.Checked then
    f3DView.Show
  else
    f3DView.Hide;
end;

procedure TfMain.Tree3DTableClick(Sender: TObject);
var
  Node: PVirtualNode;
  I: Integer;
begin
  with TVirtualStringTree(Sender) do
    begin
      Node := GetFirst;
      I := 0;
      while Node <> nil do
        begin
          if Selected[Node] then
            begin
              SceneAnimateStepTo(I);
              Break;
            end;
          Node := GetNext(Node);
          Inc(I);
        end;
    end;
end;

procedure TfMain.Tree3DTableGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSceneTableData);
end;

procedure TfMain.Tree3DTableGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data: PSceneTableData;
begin
  Data := Sender.GetNodeData(Node);
  if Column = 0 then
    CellText := Data.Value
  else
    begin
      if (Length(Data.PointChars) > 0) and (Column - 1 < Length(Data.PointChars)) then
        CellText := Data.PointChars[Column - 1]
      else
        CellText := '';
    end;
end;

procedure TfMain.TreeInfoBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data: PSceneInfoData;
  BackCol: TColor;
begin
  Data := Sender.GetNodeData(Node);
  if Data.CountMetaType <> cpatNone then
    begin
      case Data.CountMetaType of
        cpatStandart:
          BackCol := RGB(240, 255, 240);
        cpatCinamatic:
          BackCol := RGB(255, 240, 240);
        cpatDinamic:
          BackCol := RGB(255, 255, 240);
        cpatAngle:
          BackCol := RGB(240, 240, 255);
      end;
      TargetCanvas.Brush.Color := BackCol;
      TargetCanvas.Pen.Color := BackCol;
      TargetCanvas.Rectangle(CellRect);
    end
  else if Data.Caption = 'Координата' then
    begin
      BackCol := RGB(200, 255, 200);
      TargetCanvas.Brush.Color := BackCol;
      TargetCanvas.Pen.Color := BackCol;
      TargetCanvas.Rectangle(CellRect);
    end;
end;

procedure TfMain.TreeInfoGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TSceneInfoData);
end;

procedure TfMain.TreeInfoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: WideString);
var
  Data: PSceneInfoData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0: CellText := Data.Caption;
    1: CellText := Data.Value;
  end;
end;

// =====================================================================================================================
// Helper functions ----------------------------------------------------------------------------------------------------
// =====================================================================================================================

procedure TfMain.LoadHelp;
begin
  Caption := PROGRAM_NAME + ' v' + VERSION + ' [ Справка ]';
  Browser.Navigate(ExtractFilePath(Application.ExeName) + 'Help\index.html');
end;

procedure TfMain.ChangePreviewButtonImage(var btn: TSpeedButton);
begin
  btn.Glyph.Free;
  btn.Glyph := TBitmap.Create;
  fServiceDM.ImgPreview.GetBitmap(Integer(btn.Down), btn.Glyph);
  btn.Glyph.Transparent := True;
end;

procedure TfMain.ChangeStartButtonImage(var btn: TSpeedButton);
begin
  btn.Glyph.Free;
  btn.Glyph := TBitmap.Create;
  fServiceDM.ImgStartStop.GetBitmap(Integer(btn.Down), btn.Glyph);
  btn.Glyph.Transparent := True;
  if btn.Down then
    btn.Caption := 'Остановить'#13#10'испытание'
  else
    btn.Caption := 'Начать'#13#10'испытание';
end;

procedure TfMain.btnTestDebugClick(Sender: TObject);
begin
  if not fTestDebug.IsActive then
    fTestDebug.Show
  else fTestDebug.Hide;
end;

end.
