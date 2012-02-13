unit uCameraDM;

interface

uses
  SysUtils, Classes, Windows, ShellApi,
  DSPack, DSUtil, DirectShow9,
  Seuillage_inc, uConst, Math, uMCPoint;

type
  TCameraManager = class;
  TCapturePack = class;

  TFilterProperties = class
  private
    FSeuil: ISeuil;
    FFilter: TFilter;
    FOwner: TCapturePack;
    FMinPointSize: Byte;
    FPointCount: Byte;
    FTreshhold: Byte;
    FMaxPointSize: Byte;
    FActive: Boolean;
    FCallback: TOnLedDetectedCB;
    FGridSize: Byte;
    FShowGrid: Boolean;
    procedure SetGridSize(const Value: Byte);
    procedure SetShowGrid(const Value: Boolean);
    procedure SetCallback(const Value: TOnLedDetectedCB);
    procedure SetActive(const Value: Boolean);
    procedure SetMaxPointSize(const Value: Byte);
    procedure SetMinPointSize(const Value: Byte);
    procedure SetPointCount(const Value: Byte);
    procedure SetTreshhold(const Value: Byte);
  protected
    constructor Create(const AOwner: TCapturePack; const AFilter: TFilter);
    destructor Destroy; override;
    procedure Apply;
    property Callback: TOnLedDetectedCB read FCallback write SetCallback;
  public
    property Active: Boolean    read FActive       write SetActive;
    property MinPointSize: Byte read FMinPointSize write SetMinPointSize;
    property MaxPointSize: Byte read FMaxPointSize write SetMaxPointSize;
    property Treshhold: Byte    read FTreshhold    write SetTreshhold;
    property PointCount: Byte   read FPointCount   write SetPointCount;
    property ShowGrid: Boolean  read FShowGrid     write SetShowGrid;
    property GridSize: Byte     read FGridSize     write SetGridSize;
  end;

  TCapturePack = class
  private
    FEnumMT: TList;
    FOwner: TCameraManager;
    FCamera: TFilter;
    FFilter: TFilter;
    FGraph: TFilterGraph;
    FVideoWindow: TVideoWindow;
    FCameraIndex: Integer;
    FMediaTypeIndex: Integer;
    FFilterProperties: TFilterProperties;
    FIsCapturing: Boolean;
    FOnStopCapture: TNotifyEvent;
    FOnStartCapture: TNotifyEvent;
    FCameraNumber: Integer;
    function GetActive: Boolean;
    function GetIsReady: Boolean;
    function GetEnumMediaTypes: TStrings;
    function GetMediaTypeIndex: Integer;
    procedure SetCameraIndex(const Value: Integer);
    procedure SetMediaTypeIndex(const Value: Integer);
    procedure DoOnStartCapture;
    procedure DoOnStopCapture;
  protected
    constructor Create(const AOwner: TCameraManager; const ACameraNumber: Integer);
    destructor Destroy; override;
    function InitFilter: Boolean;
    procedure InitEnumMediaTypes;
    procedure ChangeMediaType;
    function Initialize(const ACamera, AFilter: TFilter; const AGraph: TFilterGraph; const AVideoWindow: TVideoWindow): Boolean;
    procedure ResetGraph;
    property CameraNumber: Integer read FCameraNumber write FCameraNumber;
    property OnStartCapture: TNotifyEvent read FOnStartCapture write FOnStartCapture;
    property OnStopCapture: TNotifyEvent read FOnStopCapture write FOnStopCapture;
  public
    procedure StartCapture;
    procedure StopCapture;
    property IsCapturing: Boolean read FIsCapturing;
    property CameraIndex: Integer read FCameraIndex write SetCameraIndex;
    property MediaTypeIndex: Integer read GetMediaTypeIndex write SetMediaTypeIndex;
    property Filter: TFilterProperties read FFilterProperties;
    property IsReady: Boolean read GetIsReady;
    property EnumMediaTypes: TStrings read GetEnumMediaTypes;
    property Active: Boolean read GetActive;
  end;

  TTwoCapturePack = array [0..1] of TCapturePack;

  TCaptureMode = (cmNone, cmSingleFirst, cmSingleSecond, cmBoth);

  TCameraSynchronizer = class
  private
    FStartTime: TDateTime;
    FCam1Points: TList;
    FCam2Points: TList;
    FOwner: TCameraManager;
    FCaptureMode: TCaptureMode;
    function GetOnCameraGetPoints(const Index: Byte): TOnLedDetectedCB;
    function Make3DList(T: Cardinal; P1, P2: TListPoint): T3DPointList;
  protected
    constructor Create(AOwner: TCameraManager);
    destructor Destroy; override;
    procedure DoOnCamera1GetPoints(Points: TListPoint);
    procedure DoOnCamera2GetPoints(Points: TListPoint);
  public
    property CameraGetPointProc[const Index: Byte]: TOnLedDetectedCB read GetOnCameraGetPoints;
    procedure SyncronizePoints;
  end;

  TSetMediaTypeEvent = procedure(Sender: TObject; const CameraIndex: Integer; Width, Height: Integer) of object;
  TBeforeStartCaptureEvent = procedure (Sender: TObject; const CameraIndex: Integer; var AllowStart: Boolean) of object;
  // When you get the point
  TGetPointsEvent = procedure (Sender: TObject; const List: T3DPointList) of object;

  TCameraManager = class
  private
    FIsCapturing: Boolean;
    FCameraEnum: TSysDevEnum;
    FCams: TTwoCapturePack;
    FOnSetMediaType: TSetMediaTypeEvent;
    FOnBeforeStartCapture: TBeforeStartCaptureEvent;
    FCamSync: TCameraSynchronizer;
    FOnGetPoint: TGetPointsEvent;
    FOnStopCapture: TNotifyEvent;
    FOnStartCapture: TNotifyEvent;
    function GetHasCameras: Boolean;
    function GetFirstCamera: TCapturePack;
    function GetSecondCamera: TCapturePack;
    function GetEnumCameras: TStrings;
    procedure InitFilter;
    procedure DoEnumCameras;
  protected
    procedure DoOnSetMediaType(const CameraIndex: Integer; Width, Height: Integer);
    procedure Initialize(const ACamera1, ACamera2, AFilter1, AFilter2: TFilter; const AGraph1, AGraph2: TFilterGraph;
      const AVideoWindow1, AVideoWindow2: TVideoWindow);
    procedure DoOnBeforeStartCapture(const CameraIndex: Integer; var AllowStart: Boolean);
    procedure DoOnGetPoint(const List: T3DPointList);
    procedure DoOnStartCapture;
    procedure DoOnStopCapture;
  public
    constructor Create;
    destructor Destroy; override;
    function StartCapture: Boolean;
    function StopCapture: Boolean;
    property IsCapturing: Boolean read FIsCapturing;
    property EnumCameras: TStrings read GetEnumCameras;
    property FirstCamera: TCapturePack read GetFirstCamera;
    property SecondCamera: TCapturePack read GetSecondCamera;
    property HasCameras: Boolean read GetHasCameras;
    property OnSetMediaType: TSetMediaTypeEvent read FOnSetMediaType write FOnSetMediaType;
    property OnBeforeStartCapture: TBeforeStartCaptureEvent read FOnBeforeStartCapture write FOnBeforeStartCapture;
    property OnStartCapture: TNotifyEvent read FOnStartCapture write FOnStartCapture;
    property OnStopCapture: TNotifyEvent read FOnStopCapture write FOnStopCapture;
    property OnGetPoint: TGetPointsEvent read FOnGetPoint write FOnGetPoint;
  end;

type
  ECameraManagerError = class (Exception);

type
  TfCameraDM = class(TDataModule)
    Seuillage1: TFilter;
    Camera1: TFilter;
    Graph1: TFilterGraph;
    Camera2: TFilter;
    Seuillage2: TFilter;
    Graph2: TFilterGraph;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure DoOnGetPoints(Sender: TObject; const List: T3DPointList);
    procedure DoOnBeforeStartCapture(Sender: TObject; const CameraIndex: Integer; var AllowStart: Boolean);
    procedure DoOnStartCature(Sender: TObject);
    procedure DoOnStopCature(Sender: TObject);
    procedure DoOnSetMediaType(Sender: TObject; const CameraIndex: Integer; Width, Height: Integer);
    procedure DoBeepOnStart;
    procedure DoLightsOnStartStop(const Num: Integer; const Value: Boolean);
    procedure DoOnStartCatureCamera(Sender: TObject);
    procedure DoOnStopCatureCamera(Sender: TObject);
  protected
    FCaptureFile: TMCFile;
    FCam1Width: Integer;
    FCam2Width: Integer;
    FCam1Height: Integer;
    FCam2Height: Integer;

    FLastStep: Integer;
    FCurStep: Integer;
    procedure ApplyCameraParams;
    procedure LoadParamsToGui;
  public
    property CaptureFile: TMCFile read FCaptureFile;
  end;

function CameraManager: TCameraManager;

var
  fCameraDM: TfCameraDM;

implementation

uses
  Forms, Registry, DateUtils, Buttons,
  uParams, uMain, u3DView, uServiceDM, uTestDebug;

const
  KEY_FREETRACKFILTER = 'CLSID\{0A99F2CA-79C9-4312-B78E-ED6CB3829275}\InprocServer32';

var
  _CameraManager: TCameraManager;

function CameraManager: TCameraManager;
begin
  Result := _CameraManager;
end;

type
  // Syncronyzer helper class
  T2DPoint = class
  private
    FTime: TDateTime;
    FPoints: TListPoint;
  public
    property Points: TListPoint read FPoints write FPoints;
    property Time: TDateTime read FTime write FTime;
  end;

{$R *.dfm}

procedure TfCameraDM.ApplyCameraParams;
begin
  with FCaptureFile, fMain do
    begin
      Options['Mass'].AsString := EditMass.Text;
      Options['Interval'].AsString := EditInterval.Text;
      Options['CameraRadius'].AsString := EditCamRadius.Text;
      Options['CameraHeight'].AsString := EditCamHeight.Text;
      Options['PointCount'].AsString := ComboPointCount.Items[ComboPointCount.ItemIndex];
      Options['TestName'].AsString := EditTestName.Text;
      Options['X0'].AsString := EditX0.Text;
      Options['Y0'].AsString := EditY0.Text;
      Options['Z0'].AsString := EditZ0.Text;
      Options['Cam1Degree'].AsString := EditCameraDegree1.Text;
      Options['Cam2Degree'].AsString := EditCameraDegree2.Text;
      Options['EnablePerspective'].AsBoolean := CheckBoxEnablePerspective.Checked;
      if FCam1Width > 0 then
        Options['Cam1ResX'].AsInteger := FCam1Width
      else
        Options['Cam1ResX'].AsInteger := 1;
      if FCam1Height > 0 then
        Options['Cam1ResY'].AsInteger := FCam1Height
      else
        Options['Cam1ResY'].AsInteger := 1;
      if FCam2Width > 0 then
        Options['Cam2ResX'].AsInteger := FCam2Width
      else
        Options['Cam2ResX'].AsInteger := 1;
      if FCam2Height > 0 then
        Options['Cam2ResY'].AsInteger := FCam2Height
      else
        Options['Cam2ResY'].AsInteger := 1;
    end;
end;

procedure TfCameraDM.DataModuleCreate(Sender: TObject);
begin
  CameraManager.Initialize(Camera1, Camera2, Seuillage1, Seuillage2, Graph1, Graph2, fMain.VideoWindow1, fMain.VideoWindow2);
  CameraManager.OnGetPoint := DoOnGetPoints;
  CameraManager.OnBeforeStartCapture := DoOnBeforeStartCapture;
  CameraManager.OnSetMediaType := DoOnSetMediaType;
  CameraManager.OnStartCapture := DoOnStartCature;
  CameraManager.OnStopCapture := DoOnStopCature;

  CameraManager.FirstCamera.OnStartCapture := DoOnStartCatureCamera;
  CameraManager.SecondCamera.OnStartCapture := DoOnStartCatureCamera;
  CameraManager.FirstCamera.OnStopCapture := DoOnStopCatureCamera;
  CameraManager.SecondCamera.OnStopCapture := DoOnStopCatureCamera;

  LoadParamsToGui;
end;

{$DEFINE DEBUG}

procedure TfCameraDM.DoOnGetPoints(Sender: TObject; const List: T3DPointList);

  function MilliSecToDateTimeFormat(const Format: String; T: Cardinal): String;
  var
    H, M, S, Ms: Word;
  begin
    H := T div 3600000;
    M := (T - H * 3600000) div 60000;
    S := (T - H * 3600000 - M * 60000) div 1000;
    Ms := T mod 1000;
    Result := FormatDateTime(Format, EncodeTime(H, M, S, Ms));
  end;

var
  I: Integer;
  P: T3DPoint;
  L: T3DPointList;
begin
  // Fix for coordinates
  L := T3DPointList.Create(List.Time);
  for I := 0 to List.Count - 1 do
    begin
      P := List[I];
      if (P.X <> -1) then
        P.X := P.X - FCam1Width div 2;
      if (P.Y <> -1) then
        P.Y := P.Y - FCam1Height div 2;
      if (P.Z <> -1) then
        P.Z := P.Z - FCam1Width div 2;
      L.Add(P);
    end;

  FCaptureFile.AddCoordinate.SetList(List);

  {$IFDEF DEBUG}
  fTestDebug.Add(List);
  {$ENDIF}

  FLastStep := FCurStep;
end;

procedure TfCameraDM.DoOnSetMediaType(Sender: TObject; const CameraIndex: Integer; Width, Height: Integer);
begin
  case CameraIndex of
    0:
      begin
        FCam1Width := Width;
        FCam1Height := Height;
      end;
    1:
      begin
        FCam2Width := Width;
        FCam2Height := Height;
      end;
  end;
end;

procedure TfCameraDM.DoBeepOnStart;
begin
  Beep(1720, 50);
end;

procedure TfCameraDM.DoLightsOnStartStop(const Num: Integer; const Value: Boolean);
var
  btn: TSpeedButton;
  oldOnClick: TNotifyEvent;
begin
  with fMain do
    begin
      btn := TSpeedButton(FindComponent('btnPreview' + IntToStr(Num + 1)));
      oldOnClick := btn.OnClick;
      btn.OnClick := nil;
      try
        btn.Down := Value;
        ChangePreviewButtonImage(btn);
      finally
        btn.OnClick := oldOnClick;
      end;
    end;
end;

procedure TfCameraDM.DoOnBeforeStartCapture(Sender: TObject; const CameraIndex: Integer; var AllowStart: Boolean);
begin
  case CameraIndex of
    0: AllowStart := fMain.ComboSource1.ItemIndex > 0;
    1: AllowStart := fMain.ComboSource2.ItemIndex > 0;
  end;
end;

procedure TfCameraDM.DoOnStartCature(Sender: TObject);
begin
  FCaptureFile := TMCFile.Create;
  ApplyCameraParams;
  FLastStep := 0;
  FCurStep := 0;
  DoBeepOnStart;
end;

procedure TfCameraDM.DoOnStartCatureCamera(Sender: TObject);
begin
  DoLightsOnStartStop(TCapturePack(Sender).CameraNumber, True);
end;

procedure TfCameraDM.DoOnStopCature(Sender: TObject);
var
  FileName: String;
begin
  if FCaptureFile.CoordinateCount = 0 then
    begin
      Application.MessageBox(PChar(cSaveWithoutPoints), nil, MB_OK or MB_ICONWARNING);
      Exit;
    end;

  FileName := ChangeFileExt(Params['TestsDir'].AsString + fMain.EditTestName.Text, '.xml');
  try
    FCaptureFile.SaveFile(FileName);
  finally
    FreeAndNil(FCaptureFile);
  end;
  fMain.StBar.Panels[1].Text := '��������� ��������� � ����: ' + FileName;
  fMain.EditTestName.Text := fServiceDM.GetFileNameByMask;
  Params['LastTest'].AsInteger := Params['LastTest'].AsInteger + 1;
end;

procedure TfCameraDM.DoOnStopCatureCamera(Sender: TObject);
begin
  DoLightsOnStartStop(TCapturePack(Sender).CameraNumber, False);
end;

procedure TfCameraDM.LoadParamsToGui;
begin
  with fMain do
    begin
      EditInterval.Text := Params['Interval'].AsString;
      CheckBoxAnimation.Checked := Params['Animation'].AsBoolean;
      CheckBoxShowCamera.Checked := Params['ShowCamera'].AsBoolean;
      ComboPointCount.ItemIndex := Params['PointCount'].AsInteger - 1;
      ComboPointCount.OnChange(ComboPointCount);

      CheckBoxSyncCamSettings.Checked := Params['SyncCamSettings'].AsBoolean;

      Treshhold1.OnChange := nil;
      Treshhold2.OnChange := nil;
      Treshhold1Edit.OnChange := nil;
      Treshhold2Edit.OnChange := nil;

      MinPointSize1.OnChange := nil;
      MinPointSize2.OnChange := nil;
      MaxPointSize1.OnChange := nil;
      MaxPointSize2.OnChange := nil;

      MinPointSize1Edit.OnChange := nil;
      MinPointSize2Edit.OnChange := nil;
      MaxPointSize1Edit.OnChange := nil;
      MaxPointSize2Edit.OnChange := nil;

      Treshhold1.Position := Params['Treshhold1'].AsInteger;
      Treshhold2.Position := Params['Treshhold2'].AsInteger;
      MinPointSize1.Position := Params['MinPointSize2'].AsInteger;
      MinPointSize2.Position := Params['MinPointSize1'].AsInteger;
      MaxPointSize1.Position := Params['MaxPointSize2'].AsInteger;
      MaxPointSize2.Position := Params['MaxPointSize1'].AsInteger;

      Treshhold1UpDown.Position    := Treshhold1.Position;
      Treshhold2UpDown.Position    := Treshhold2.Position;
      MinPointSize1UpDown.Position := MinPointSize1.Position;
      MinPointSize2UpDown.Position := MinPointSize2.Position;
      MaxPointSize1UpDown.Position := MaxPointSize1.Position;
      MaxPointSize2UpDown.Position := MaxPointSize2.Position;

      MinPointSize1.OnChange := FilterPropertyChange;
      MinPointSize2.OnChange := FilterPropertyChange;
      MaxPointSize1.OnChange := FilterPropertyChange;
      MaxPointSize2.OnChange := FilterPropertyChange;

      MinPointSize1Edit.OnChange := FilterPropertyChange;
      MinPointSize2Edit.OnChange := FilterPropertyChange;
      MaxPointSize1Edit.OnChange := FilterPropertyChange;
      MaxPointSize2Edit.OnChange := FilterPropertyChange;

      Treshhold1.OnChange := FilterPropertyChange;
      Treshhold2.OnChange := FilterPropertyChange;
      Treshhold1Edit.OnChange := FilterPropertyChange;
      Treshhold2Edit.OnChange := FilterPropertyChange;

      CheckBoxDisableFilter1.Checked := Params['DisableFilter1'].AsBoolean;
      CheckBoxDisableFilter2.Checked := Params['DisableFilter2'].AsBoolean;
      CheckBoxGrid1.Checked := Params['ShowGrid1'].AsBoolean;
      CheckBoxGrid2.Checked := Params['ShowGrid2'].AsBoolean;
      EditCameraDegree1.Text := Params['CameraDegree1'].AsString;
      EditCameraDegree2.Text := Params['CameraDegree2'].AsString;
      EditCamRadius.Text := Params['CamRadius'].AsString;
      EditCamHeight.Text := Params['CamHeight'].AsString;
      EditX0.Text := Params['X0'].AsString;
      EditY0.Text := Params['Y0'].AsString;
      EditZ0.Text := Params['Z0'].AsString;
      EditMass.Text := Params['Mass'].AsString;

      CheckBoxEnablePerspective.Checked := Params['EnablePerspective'].AsBoolean;
    end;
end;

{ ================================================================================================ }
{ --------------------------------------- TFilterProperties -------------------------------------- }
{ ================================================================================================ }

procedure TFilterProperties.Apply;
begin
  FSeuil := nil;
  FFilter.QueryInterface(IID_ISeuil, FSeuil);
  if Assigned(FSeuil) then
    begin
      FSeuil.SetActive(FActive);
      FSeuil.SetMaxPointSize(FMaxPointSize);
      FSeuil.SetMinPointSize(FMinPointSize);
      FSeuil.SetSeuil(FTreshhold);
      FSeuil.SetMaxPointCount(FPointCount);
      FSeuil.SetCallback(FCallback);
      FSeuil.SetShowGrid(FShowGrid);
      FSeuil.SetGridSize(FGridSize);
    end;
end;

constructor TFilterProperties.Create(const AOwner: TCapturePack; const AFilter: TFilter);
begin
  if AFilter = nil then
    begin
      FreeAndNil(Self);
      raise ECameraManagerError.CreateFmt(cFilterIsNull, [FOwner.CameraIndex]);
    end;
  FOwner := AOwner;
  FFilter := AFilter;
  FFilter.QueryInterface(IID_ISeuil, FSeuil);
  FActive := True;
  FPointCount := 1;
  FTreshhold := 200;
  FMinPointSize := 2;
  FMaxPointSize := 50;
  FShowGrid := True;
  FGridSize := 4;
end;

destructor TFilterProperties.Destroy;
begin
  FFilter := nil;
  FOwner := nil;
  FSeuil := nil;
  inherited;
end;

procedure TFilterProperties.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetCallback(const Value: TOnLedDetectedCB);
begin
  FCallback := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetGridSize(const Value: Byte);
begin
  FGridSize := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetMaxPointSize(const Value: Byte);
begin
  FMaxPointSize := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetMinPointSize(const Value: Byte);
begin
  FMinPointSize := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetShowGrid(const Value: Boolean);
begin
  FShowGrid := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetPointCount(const Value: Byte);
begin
  FPointCount := Value;
  if FOwner.IsCapturing then
    Apply;
end;

procedure TFilterProperties.SetTreshhold(const Value: Byte);
begin
  FTreshhold := Value;
  if FOwner.IsCapturing then
    Apply;
end;

{ ================================================================================================ }
{ ----------------------------------------- TCapturePack ----------------------------------------- }
{ ����� ������������� ������, �������, ������� �����... }
{ ... �������� ��������... }
{ ================================================================================================ }

function SubTypeToStr(const SubType: TGUID): String;
begin
  if IsEqualGUID(SubType, MEDIASUBTYPE_RGB8) then
    begin
      Result := 'RGB8';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YVU9) then
    begin
      Result := 'YVU9';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IF09) then
    begin
      Result := 'IF09';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YV12) then
    begin
      Result := 'YV12';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IYUV) then
    begin
      Result := 'IYUV';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_NV12) then
    begin
      Result := 'NV12';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IMC1) then
    begin
      Result := 'IMC1';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IMC2) then
    begin
      Result := 'IMC2';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IMC3) then
    begin
      Result := 'IMC3';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_IMC4) then
    begin
      Result := 'IMC4';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_RGB8) then
    begin
      Result := 'RGB8';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_RGB24) then
    begin
      Result := 'RGB24';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_RGB32) then
    begin
      Result := 'RGB32';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YUYV) then
    begin
      Result := 'YUYV';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YUY2) then
    begin
      Result := 'YUY2';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YVYU) then
    begin
      Result := 'YVYU';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_UYVY) then
    begin
      Result := 'UYVY';
      Exit;
    end;
  if IsEqualGUID(SubType, Seuillage_inc.MEDIASUBTYPE_I420) then
    begin
      Result := 'I420';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_Y800) then
    begin
      Result := 'Y800';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_Y8) then
    begin
      Result := 'Y8';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_Y160) then
    begin
      Result := 'Y160';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_YV16) then
    begin
      Result := 'YV16';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_Y422) then
    begin
      Result := 'Y422';
      Exit;
    end;
  if IsEqualGUID(SubType, MEDIASUBTYPE_GREY) then
    begin
      Result := 'GREY';
      Exit;
    end;
  Result := '';
end;

procedure TCapturePack.ChangeMediaType;
var
  VideoPin: IPin;
  StreamConfig: IAMStreamConfig;
  pmt: PAMMediaType;
  HR: HRESULT;
  sErr: String;
begin
  FGraph.Active := True;
  if CheckDSError((FGraph as ICaptureGraphBuilder2).FindPin(FCamera, PINDIR_OUTPUT,
        @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, False, 0, VideoPin)) = S_OK then
    begin
      if VideoPin.QueryInterface(IAMStreamConfig, StreamConfig) = S_OK then
        begin
          pmt := PAMMediaType(FEnumMT[FMediaTypeIndex]);
          HR := StreamConfig.SetFormat(TAMMediaType(pmt^));
          if HR = S_OK then
            with TVideoInfoHeader(pmt.pbFormat^).bmiHeader do
              FOwner.DoOnSetMediaType(FCameraIndex, biWidth, biHeight)
          else
            begin
              sErr := '0x' + IntToHex(HR, 8);
              raise ECameraManagerError.CreateFmt(cApplyMediaTypeError, [FCameraIndex, sErr]);
            end;
        end
      else
        raise ECameraManagerError.CreateFmt(cUnableQueryInterface, ['IAMStreamConfig', FCameraIndex]);
    end
  else
    raise ECameraManagerError.CreateFmt(cUnableToGetCaptureVideoPin, [FCameraIndex + 1]);
end;

constructor TCapturePack.Create(const AOwner: TCameraManager; const ACameraNumber: Integer);
begin
  FOwner := AOwner;
  FCameraIndex := -1;
  FMediaTypeIndex := -1;
  FFilterProperties := nil;
  FCameraNumber := ACameraNumber;
end;

destructor TCapturePack.Destroy;
begin
  FOwner := nil;
  FCamera := nil;
  FFilter := nil;
  FGraph := nil;
  FVideoWindow := nil;

  if Assigned(FFilterProperties) then
    FreeAndNil(FFilterProperties);
  inherited;
end;

procedure TCapturePack.DoOnStartCapture;
begin
  if Assigned(FOnStartCapture) then
    FOnStartCapture(Self);
end;

procedure TCapturePack.DoOnStopCapture;
begin
  if Assigned(FOnStopCapture) then
    FOnStopCapture(Self);
end;

function TCapturePack.GetActive: Boolean;
begin
  Result := FCameraIndex >= 0;
end;

function TCapturePack.GetEnumMediaTypes: TStrings;
var
  I: Integer;
  pmt: PAMMediaType;
  S: String;
begin
  Result := nil;
  if Assigned(FEnumMT) then
    begin
      Result := TStringList.Create;
      for I := 0 to FEnumMT.Count - 1 do
        begin
          pmt := PAMMediaType(FEnumMT[I]);
          S := IntToStr(PVideoInfoHeader(pmt.pbFormat).bmiHeader.biWidth) + 'x' +
            IntToStr(PVideoInfoHeader(pmt.pbFormat).bmiHeader.biHeight) + ' ' +
            IntToStr(PVideoInfoHeader(pmt.pbFormat).bmiHeader.biBitCount) + ' bit (' + SubTypeToStr(pmt.subtype) + ')';
          Result.Add(S);
        end;
    end;
end;

function TCapturePack.GetIsReady: Boolean;
begin
  Result := Assigned(FCamera) and Assigned(FFilter) and Assigned(FGraph) and (FCameraIndex >= 0);
end;

function TCapturePack.GetMediaTypeIndex: Integer;
begin
  Result := FMediaTypeIndex;
end;

procedure TCapturePack.InitEnumMediaTypes;
var
  VideoPin: IPin;
  pmt: PAMMediaType;
  EnumMediaTypes: IEnumMediaTypes;
begin
  if not IsReady then
    raise ECameraManagerError.CreateFmt(cEnumMediaTypesError, [FCameraIndex - 1]);

  if Assigned(FEnumMT) then
    FreeAndNil(FEnumMT);

  FEnumMT := TList.Create;

  if CheckDSError((FGraph as ICaptureGraphBuilder2).FindPin(FCamera, PINDIR_OUTPUT,
        @PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, False, 0, VideoPin)) = S_OK then
    begin
      if (VideoPin.EnumMediaTypes(EnumMediaTypes) = S_OK) then
        begin
          while (EnumMediaTypes.Next(1, pmt, nil) = S_OK) do
            begin
              if IsEqualGUID(pmt.formattype, FORMAT_VideoInfo) and (SubTypeToStr(pmt.subtype) <> '') then
                FEnumMT.Add(pmt);
            end;
          if FEnumMT.Count = 0 then
            raise ECameraManagerError.CreateFmt(cNoMediaTypes, [FCameraIndex - 1]);
        end
      else
        raise ECameraManagerError.CreateFmt(cUnableToGetEnumMediaTypes, [FCameraIndex - 1]);
    end
  else
    raise ECameraManagerError.CreateFmt(cUnableToGetCaptureVideoPin, [FCameraIndex - 1])
end;

function TCapturePack.InitFilter: Boolean;
var
  Seuil: ISeuil;
begin
  Result := False;

  Seuil := nil;
  FGraph.Active := True;
  FFilter.QueryInterface(IID_ISeuil, Seuil);
  if Seuil = nil then
    begin
      // If filter couldn't be connected, then throw away
      FGraph.Active := False;
      Exit;
    end;

  FFilterProperties.Apply;

  // If reached here, then all is ok
  Result := True;
end;

function TCapturePack.Initialize(const ACamera, AFilter: TFilter; const AGraph: TFilterGraph; const AVideoWindow: TVideoWindow): Boolean;
begin
  FCamera := ACamera;
  FFilter := AFilter;
  FGraph := AGraph;
  FVideoWindow := AVideoWindow;

  FFilterProperties := TFilterProperties.Create(Self, FFilter);

  Result := InitFilter;
end;

procedure TCapturePack.ResetGraph;
var
  FOldMediaType: Integer;
begin
  FGraph.ClearGraph;
  FGraph.Active := False;
  FCamera.BaseFilter.Moniker := nil;
  FCamera.BaseFilter.Moniker := FOwner.FCameraEnum.GetMoniker(FCameraIndex);
  FGraph.Active := True;

  FOldMediaType := FMediaTypeIndex;
  InitEnumMediaTypes;
  if FEnumMT.Count > 0 then
    begin
      SetMediaTypeIndex(0);
      SetMediaTypeIndex(FOldMediaType);
    end;

  InitFilter;

  FVideoWindow.CheckInputPinsConnected;
end;

procedure TCapturePack.SetCameraIndex(const Value: Integer);
begin
  if not InRange(Value, -1, FOwner.FCameraEnum.CountFilters - 1) then
    raise ECameraManagerError.CreateFmt(cCameraIndexOutOfRange, [Value]);

  FCameraIndex := Value;

  if Active then
    begin
      FGraph.ClearGraph;
      FGraph.Active := False;
      if Assigned(FCamera.BaseFilter.Moniker) then
        FCamera.BaseFilter.Moniker := nil;
      FCamera.BaseFilter.Moniker := FOwner.FCameraEnum.GetMoniker(FCameraIndex);
      FGraph.Active := True;

      InitEnumMediaTypes;
    end;
end;

procedure TCapturePack.SetMediaTypeIndex(const Value: Integer);
begin
  if not InRange(Value, 0, FEnumMT.Count - 1) then
    raise ECameraManagerError.CreateFmt(cMediaTypeIndexOutOfRange, [Value]);

  FMediaTypeIndex := Value;

  try
    ChangeMediaType;
  except
    on Ec: ECameraManagerError do
      raise Ec;
    on Ee: Exception do
      raise ECameraManagerError.CreateFmt('New outer exception %s', [Ee.ClassName + ': ' + Ee.Message]);
  end;
end;

procedure TCapturePack.StartCapture;
var
  HR: HRESULT;
begin
  if not IsReady then
    Exit;

  ResetGraph;
  FIsCapturing := False;

  with FGraph as ICaptureGraphBuilder2 do
    if FCamera.BaseFilter.DataLength > 0 then
      begin
        HR := RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCamera as IBaseFilter,
          FFilter as IBaseFilter, FVideoWindow as IBaseFilter);
        if HR = S_OK then
          begin
            try
              FIsCapturing := FGraph.Play;
            finally
              if FIsCapturing then
                DoOnStartCapture;
            end;
          end
        else
          raise ECameraManagerError.CreateFmt(cStartVideoError, [FCameraIndex, IntToHex(HR, 8)]);
      end;
end;

procedure TCapturePack.StopCapture;
var
  Seuil: ISeuil;
begin
  if not FIsCapturing then
    Exit;

  try
    FFilter.QueryInterface(IID_ISeuil, Seuil);
    if Assigned(Seuil) then
      begin
        Filter.Callback := nil;
        Seuil.SetCallback(nil);
        Seuil.SetActive(False);
      end;
    FGraph.ClearGraph;
    FGraph.Active := False;
  finally
    FIsCapturing := False;
    DoOnStopCapture;
  end;
end;

{ ================================================================================================ }
{ -------------------------------------- TCameraSynchronizer ------------------------------------- }
{ ================================================================================================ }

constructor TCameraSynchronizer.Create(AOwner: TCameraManager);
begin
  FOwner := AOwner;
  FCam1Points := TList.Create;
  FCam2Points := TList.Create;
  FCaptureMode := cmNone;
end;

destructor TCameraSynchronizer.Destroy;
begin
  FOwner := nil;
  FreeAndNil(FCam1Points);
  FreeAndNil(FCam2Points);
  inherited;
end;

procedure TCameraSynchronizer.DoOnCamera1GetPoints(Points: TListPoint);
var
  P: T2DPoint;
begin
  if FOwner.IsCapturing then
    begin
      P := T2DPoint.Create;
      P.Time := Now;
      P.Points := Points.Clone;
      FCam1Points.Add(P);

      {$IFDEF DEBUG}
      if Points.Count > 0 then
        fTestDebug.Add('Cam1: ' + FloatToStr(Points[0].X) + ', ' + FloatToStr(Points[0].Y));
      {$ENDIF}
    end;
end;

procedure TCameraSynchronizer.DoOnCamera2GetPoints(Points: TListPoint);
var
  P: T2DPoint;
begin
  if FOwner.IsCapturing then
    begin
      P := T2DPoint.Create;
      P.Time := Now;
      P.Points := Points.Clone;

      {$IFDEF DEBUG}
      if Points.Count > 0 then
        fTestDebug.Add('Cam2: ' + FloatToStr(Points[0].X) + ', ' + FloatToStr(Points[0].Y));
      {$ENDIF}
    end;
end;

function TCameraSynchronizer.GetOnCameraGetPoints(const Index: Byte): TOnLedDetectedCB;
begin
  Result := nil;
  case Index of
    0: Result := DoOnCamera1GetPoints;
    1: Result := DoOnCamera2GetPoints;
  end;
end;

function TCameraSynchronizer.Make3DList(T: Cardinal; P1, P2: TListPoint): T3DPointList;

  procedure FillZeroPoints(const PointCount: Integer; var Points: T3DPointList);
  var
    P: T3DPoint;
    ToFill, I: Integer;
  begin
    ToFill := PointCount - Points.Count;
    P.X := -1;
    P.Y := -1;
    P.Z := -1;
    for I := 0 to ToFill - 1 do
      Points.Add(P);
  end;

var
  I, L: Integer;
  P: T3DPoint;
begin
  Result := T3DPointList.Create(T);
  case FCaptureMode of
    cmSingleFirst:
      for I := 0 to P1.Count - 1 do
        begin
          P.X := P1[I].X;
          P.Y := -1;
          P.Z := P1[I].Y;
          Result.Add(P);
        end;
    cmSingleSecond:
      for I := 0 to P2.Count - 1 do
        begin
          P.X := -1;
          P.Y := P2[I].X;
          P.Z := P2[I].Y;
          Result.Add(P);
        end;
    cmBoth:
      begin
        if P1.Count > P2.Count then
          L := P2.Count
        else
          L := P1.Count;
        for I := 0 to L - 1 do
          begin
            P.X := P1[I].X;
            P.Y := P2[I].X;
            P.Z := P1[I].Y;
            Result.Add(P);
          end;
      end;
    cmNone: // Should not be so...
      raise ECameraManagerError.Create(cSynchronizeNoneModeError);
  end;
  FillZeroPoints(Params['PointCount'].AsInteger, Result);
end;

procedure TCameraSynchronizer.SyncronizePoints;

  function FindClosest(const P: T2DPoint; const InList: TList): T2DPoint;
  var
    I: Integer;
    PrevDiff, Diff: Cardinal;
  begin
    if InList.Count > 1 then
      begin
        I := 1;
        while I < InList.Count do
          begin
            PrevDiff := MilliSecondsBetween(P.Time, T2DPoint(InList[I - 1]).Time);
            Diff := MilliSecondsBetween(P.Time, T2DPoint(InList[I]).Time);
            if PrevDiff < Diff then
              Break;
            Inc(I);
          end;
        Result := T2DPoint(InList[I - 1]);
      end
    else
      Result := P;
  end;

var
  P1, P2: T2DPoint;
  I: Integer;
  T: Cardinal;
  Point: T3DPointList;
  ForList: TList;
  PrevTime: TDateTime;
begin
  case FCaptureMode of
    cmSingleFirst, cmBoth:
      ForList := FCam1Points;
    cmSingleSecond:
      ForList := FCam2Points;
    cmNone:
      raise ECameraManagerError.Create(cSynchronizeNoneModeError);
  end;

  if ForList.Count > 0 then
    begin
      PrevTime := FStartTime;
      for I := 0 to ForList.Count - 1 do
        begin
          case FCaptureMode of
            cmSingleFirst:
              begin
                P1 := T2DPoint(ForList[I]);
                T := MilliSecondsBetween(PrevTime, P1.Time);
                Point := Make3DList(T, P1.Points, nil);
                PrevTime := P1.Time;
              end;
            cmSingleSecond:
              begin
                P2 := T2DPoint(ForList[I]);
                T := MilliSecondsBetween(PrevTime, P1.Time);
                Point := Make3DList(T, nil, P2.Points);
                PrevTime := P2.Time;
              end;
            cmBoth:
              begin
                P1 := T2DPoint(ForList[I]);
                P2 := FindClosest(P1, FCam2Points);
                T := MilliSecondsBetween(PrevTime, P1.Time + (P1.Time - P2.Time));
                Point := Make3DList(T, P1.Points, P2.Points);
                PrevTime := P1.Time + (P1.Time - P2.Time);
              end;
          end;
          FOwner.DoOnGetPoint(Point);
        end;
    end;
end;

{ ================================================================================================ }
{ ---------------------------------------- TCameraManager ---------------------------------------- }
{ ================================================================================================ }

procedure TCameraManager.InitFilter;
var
  Key: TRegistry;
  Location, FilterFile: String;
begin
  Key := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Key.RootKey := HKEY_CLASSES_ROOT;
    if Key.OpenKey(KEY_FREETRACKFILTER, True) then
      Location := Key.ReadString('') // read default
    else
      Location := '';
    Key.CloseKey;
  finally
    Key.Free;
  end;

  FilterFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'FreeTrackFilterSSE2.ax';

  if (Location = '') or (Location <> FilterFile) then
    if FileExists(FilterFile) then
      begin
        ShellExecute(0, 'open', 'Regsvr32', PChar('/s "' + FilterFile ), '', SW_HIDE);
        LoadLibrary(PChar(filterFile));
        Sleep(100); // give it a chance  // =)
      end;
end;

constructor TCameraManager.Create;
var
  I: Integer;
begin
  FCameraEnum := nil;
  FCamSync := TCameraSynchronizer.Create(Self);
  for I := 0 to 1 do
    FCams[I] := TCapturePack.Create(Self, I);

  FIsCapturing := False;
end;

destructor TCameraManager.Destroy;
begin
  StopCapture;

  FreeAndNil(FCams[0]);
  FreeAndNil(FCams[1]);
  if Assigned(FCameraEnum) then
    FreeAndNil(FCameraEnum);
  if Assigned(FCamSync) then
    FreeAndNil(FCamSync);
  inherited;
end;

procedure TCameraManager.DoEnumCameras;
begin
  if FCameraEnum = nil then
    FCameraEnum := TSysDevEnum.Create(CLSID_VideoInputDeviceCategory);
end;

procedure TCameraManager.DoOnBeforeStartCapture(const CameraIndex: Integer; var AllowStart: Boolean);
begin
  if Assigned(FOnBeforeStartCapture) then
    FOnBeforeStartCapture(Self, CameraIndex, AllowStart);
end;

procedure TCameraManager.DoOnStartCapture;
begin
  if Assigned(FOnStartCapture) then
    FOnStartCapture(Self);
end;

procedure TCameraManager.DoOnStopCapture;
begin
  if Assigned(FOnStopCapture) then
    FOnStopCapture(Self);
end;

function TCameraManager.GetEnumCameras: TStrings;
var
  I: Integer;
begin
  DoEnumCameras;

  Result := TStringList.Create;

  Result.Add('���');
  for I := 0 to FCameraEnum.CountFilters - 1 do
    Result.Add(IntToStr(I) + ' - ' + FCameraEnum.Filters[I].FriendlyName);
end;

function TCameraManager.GetFirstCamera: TCapturePack;
begin
  Result := FCams[0];
end;

function TCameraManager.GetHasCameras: Boolean;
begin
  Result := FCameraEnum.CountFilters > 0; 
end;

function TCameraManager.GetSecondCamera: TCapturePack;
begin
  Result := FCams[1];
end;

procedure TCameraManager.Initialize(const ACamera1, ACamera2, AFilter1, AFilter2: TFilter; const AGraph1,
  AGraph2: TFilterGraph; const AVideoWindow1, AVideoWindow2: TVideoWindow);
var
  Res: Boolean;
begin
  DoEnumCameras;
  if HasCameras then
    begin
      InitFilter;
      Res := FCams[0].Initialize(ACamera1, AFilter1, AGraph1, AVideoWindow1);
      Res := Res and FCams[1].Initialize(ACamera2, AFilter2, AGraph2, AVideoWindow2);
      if not Res then
        raise ECameraManagerError.Create(cFilterLoadError);
    end;
end;

procedure TCameraManager.DoOnGetPoint(const List: T3DPointList);
begin
  if Assigned(FOnGetPoint) then
    FOnGetPoint(Self, List);
end;

procedure TCameraManager.DoOnSetMediaType(const CameraIndex: Integer; Width, Height: Integer);
begin
  if Assigned(FOnSetMediaType) then
    FOnSetMediaType(Self, CameraIndex, Width, Height);
end;

function TCameraManager.StartCapture: Boolean;
var
  I: Integer;
  VideoWindow: IBaseFilter;
  AllowStart: Boolean;
begin
  Result := True;
  if FCams[0].Active and FCams[1].Active and FCams[0].Filter.Active and FCams[1].Filter.Active then
    FCamSync.FCaptureMode := cmBoth
  else if FCams[0].Active and FCams[0].Filter.Active then
    FCamSync.FCaptureMode := cmSingleFirst
  else if FCams[1].Active and FCams[1].Filter.Active then
    FCamSync.FCaptureMode := cmSingleSecond
  else
    begin
      Application.MessageBox(PChar(cStartCaptureWithoutFilter), nil, MB_OK or MB_ICONWARNING);
      Result := False;
      Exit;
    end;

  for I := 0 to 1 do
    begin
      VideoWindow := nil;
      AllowStart := True;
      DoOnBeforeStartCapture(I, AllowStart);
      if AllowStart then
        begin
          if FCams[I].Filter.Active then
            FCams[I].Filter.Callback := FCamSync.CameraGetPointProc[I];
          FCams[I].StartCapture;
        end;
    end;
  DoOnStartCapture;
  FIsCapturing := True;
  FCamSync.FStartTime := Now;
end;

function TCameraManager.StopCapture: Boolean;
var
  I: Integer;
begin
  Result := True;
  if not FIsCapturing then
    begin
      Result := False;
      Exit;
    end;

  for I := 0 to 1 do
    FCams[I].StopCapture;
  FCamSync.SyncronizePoints;
  DoOnStopCapture;
  FIsCapturing := False;
end;

initialization

  _CameraManager := TCameraManager.Create;

finalization

  FreeAndNil(_CameraManager);

end.
