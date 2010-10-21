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
    procedure SetActive(const Value: Boolean);
    procedure SetMaxPointSize(const Value: Byte);
    procedure SetMinPointSize(const Value: Byte);
    procedure SetPointCount(const Value: Byte);
    procedure SetTreshhold(const Value: Byte);
  protected
    constructor Create(const AOwner: TCapturePack; const AFilter: TFilter);
    destructor Destroy; override;
    procedure Apply;
    property Callback: TOnLedDetectedCB read FCallback write FCallback;
  public
    property Active: Boolean    read FActive       write SetActive;
    property MinPointSize: Byte read FMinPointSize write SetMinPointSize;
    property MaxPointSize: Byte read FMaxPointSize write SetMaxPointSize;
    property Treshhold: Byte    read FTreshhold    write SetTreshhold;
    property PointCount: Byte   read FPointCount   write SetPointCount;
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
    function GetActive: Boolean;
    function GetIsReady: Boolean;
    function GetEnumMediaTypes: TStrings;
    function GetMediaTypeIndex: Integer;
    procedure SetCameraIndex(const Value: Integer);
    procedure SetMediaTypeIndex(const Value: Integer);
  protected
    constructor Create(const AOwner: TCameraManager);
    destructor Destroy; override;
    function InitFilter: Boolean;
    procedure InitEnumMediaTypes;
    procedure ChangeMediaType;
    function Initialize(const ACamera, AFilter: TFilter; const AGraph: TFilterGraph; const AVideoWindow: TVideoWindow): Boolean;
    procedure ResetGraph;
    property IsCapturing: Boolean read FIsCapturing;
  public
    procedure StartCapture;
    procedure StopCapture;
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
    FPoints: T3DPointList;
    FOwner: TCameraManager;
    FCaptureMode: TCaptureMode;

    FPrevSyncTime: TDateTime;

    FCamera1Points: TListPoint;
    FCamera2Points: TListPoint;
    FCamera1Time: TDateTime;
    FCamera2Time: TDateTime;
    FCamera1LastTime: TDateTime;
    FCamera2LastTime: TDateTime;
    function GetOnCameraGetPoints(const Index: Byte): TOnLedDetectedCB;

    procedure Make3DList;
  protected
    constructor Create(AOwner: TCameraManager);
    destructor Destroy; override;
    procedure DoSynchronize;
    procedure Drop;
    procedure DoOnCamera1GetPoints(Points: TListPoint);
    procedure DoOnCamera2GetPoints(Points: TListPoint);
  public
    property CameraGetPointProc[const Index: Byte]: TOnLedDetectedCB read GetOnCameraGetPoints;
  end;

  TSetMediaTypeEvent = procedure(Sender: TObject; const CameraIndex: Integer; Width, Height: Integer) of object;
  TBeforeStartCaptureEvent = procedure (Sender: TObject; const CameraIndex: Integer; var AllowStart: Boolean) of object;
  // When you get the point
  TGetPointsEvent = procedure (Sender: TObject; const List: T3DPointList) of object;

  TCameraManager = class
  private
    FCameraEnum: TSysDevEnum;
    FCams: TTwoCapturePack;
    FOnSetMediaType: TSetMediaTypeEvent;
    FOnBeforeStartCapture: TBeforeStartCaptureEvent;
    FCamSync: TCameraSynchronizer;
    FOnGetPoint: TGetPointsEvent;
    FOnStopCapture: TNotifyEvent;
    FOnStartCapture: TNotifyEvent;
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
    procedure StartCapture;
    procedure StopCapture;
    property EnumCameras: TStrings read GetEnumCameras;
    property FirstCamera: TCapturePack read GetFirstCamera;
    property SecondCamera: TCapturePack read GetSecondCamera;
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
  protected
    FCaptureFile: TMCFile;
    FCam1Width: Integer;
    FCam2Width: Integer;
    FCam1Height: Integer;
    FCam2Height: Integer;
    FTestTime: Cardinal;

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
  Forms, Registry, DateUtils, uParams, uMain, u3DView, uServiceDM;

const
  KEY_FREETRACKFILTER = 'CLSID\{0A99F2CA-79C9-4312-B78E-ED6CB3829275}\InprocServer32';

var
  _CameraManager: TCameraManager;

function CameraManager: TCameraManager;
begin
  Result := _CameraManager;
end;

{$R *.dfm}

procedure TfCameraDM.ApplyCameraParams;
begin
  with FCaptureFile, fMain do
    begin
      Options['Mass'].AsString := EditMass.Text;
      Options['LineColor'].AsString := EditColor.Text;
      Options['Interval'].AsString := EditInterval.Text;
      Options['CameraRadius'].AsString := EditCamRadius.Text;
      Options['CameraHeight'].AsString := EditCamHeight.Text;
      Options['PointCount'].AsString := ComboPointCount.Items[ComboPointCount.ItemIndex];
      Options['TestName'].AsString := EditTestName.Text;
      Options['X0'].AsString := EditX0.Text;
      Options['Y0'].AsString := EditY0.Text;
      Options['Z0'].AsString := EditZ0.Text;
      Options['EnablePerspective'].AsBoolean := CheckBoxEnablePerspective.Checked;
      Options['Cam1Degree'].AsString := EditCameraDegree1.Text;
      Options['Cam2Degree'].AsString := EditCameraDegree2.Text;
      Options['Cam1ResX'].AsInteger := FCam1Width;
      Options['Cam1ResY'].AsInteger := FCam1Height;
      Options['Cam2ResX'].AsInteger := FCam2Width;
      Options['Cam2ResY'].AsInteger := FCam2Height;
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

  LoadParamsToGui;
end;

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

begin
  with FCaptureFile.AddCoordinate do
    begin
      //FCurStep := FCaptureFile.CoordinateCount - 1;
      SetList(List);
    end;

  //f3DView.SceneStepForvard(FLastStep, FCurStep);
  FLastStep := FCurStep;
  Inc(FTestTime, List.Time);
  fMain.StBar.Panels[0].Text := 'Время: ' + MilliSecToDateTimeFormat('hh:nn:ss.zzz', FTestTime);
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
  //f3DView.ScenePrepare;
  FLastStep := 0;
  FCurStep := 0;
end;

procedure TfCameraDM.DoOnStopCature(Sender: TObject);
var
  FileName: String;
begin
  FileName := ChangeFileExt(fMain.TestsDir + fMain.EditTestName.Text, '.xml');
  try
    FCaptureFile.SaveFile(FileName);
  finally
    FreeAndNil(FCaptureFile);
  end;
  fMain.StBar.Panels[1].Text := 'Испытание сохранено в файл: ' + FileName;
  fMain.EditTestName.Text := fServiceDM.GetLastFile(fMain.TestsDir + 'Испытание *.xml');
end;

procedure TfCameraDM.LoadParamsToGui;
begin
  with fMain do
    begin
      EditInterval.Text := Params['Interval'].AsString;
      CheckBoxAnimation.Checked := Params['Animation'].AsBoolean;
      CheckBoxShowCamera.Checked := Params['ShowCamera'].AsBoolean;
      ComboPointCount.ItemIndex := Params['PointCount'].AsInteger - 1;

      Treshhold1.Position := Params['Treshhold1'].AsInteger;
      Treshhold2.Position := Params['Treshhold2'].AsInteger;
      MinPointSize2.Position := Params['MinPointSize2'].AsInteger;
      MinPointSize1.Position := Params['MinPointSize1'].AsInteger;
      MaxPointSize2.Position := Params['MaxPointSize2'].AsInteger;
      MaxPointSize1.Position := Params['MaxPointSize1'].AsInteger;

      Treshhold1UpDown.Position    := Treshhold1.Position;
      Treshhold2UpDown.Position    := Treshhold2.Position;
      MinPointSize2UpDown.Position := MinPointSize2.Position;
      MinPointSize1UpDown.Position := MinPointSize1.Position;
      MaxPointSize2UpDown.Position := MaxPointSize2.Position;
      MaxPointSize1UpDown.Position := MaxPointSize1.Position;

      CheckBoxDisableFilter1.Checked := Params['DisableFilter2'].AsBoolean;
      CheckBoxDisableFilter2.Checked := Params['DisableFilter1'].AsBoolean;
      EditCameraDegree1.Text := Params['CameraDegree1'].AsString;
      EditCameraDegree2.Text := Params['CameraDegree2'].AsString;
      EditCamRadius.Text := Params['CamRadius'].AsString;
      EditCamHeight.Text := Params['CamHeight'].AsString;
      EditX0.Text := Params['X0'].AsString;
      EditY0.Text := Params['Y0'].AsString;
      EditZ0.Text := Params['Z0'].AsString;
      EditMass.Text := Params['Mass'].AsString;
      EditColor.Text := Params['Color'].AsString;
      CheckBoxEnablePerspective.Checked := Params['EnablePerspective'].AsBoolean;
      CheckBoxSyncCamSettings.Checked := Params['SyncCamSettings'].AsBoolean;
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
{ Здесь настраиваются камеры, фильтры, стоятся графы... }
{ ... грабятся корованы... }
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

constructor TCapturePack.Create(const AOwner: TCameraManager);
begin
  FOwner := AOwner;
  FCameraIndex := -1;
  FMediaTypeIndex := -1;
  FFilterProperties := nil;
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
              if IsEqualGUID(pmt.formattype, FORMAT_VideoInfo) then
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

  ChangeMediaType;
end;

procedure TCapturePack.StartCapture;
var
  HR: HRESULT;
begin
  if not IsReady then
    Exit;

  ResetGraph;

  with FGraph as ICaptureGraphBuilder2 do
    if FCamera.BaseFilter.DataLength > 0 then
      begin
        HR := RenderStream(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, FCamera as IBaseFilter,
          FFilter as IBaseFilter, FVideoWindow as IBaseFilter);
        if HR = S_OK then
          FIsCapturing := FGraph.Play
        else
          raise ECameraManagerError.CreateFmt(cStartVideoError, [FCameraIndex, IntToHex(HR, 8)]);
      end;
end;

procedure TCapturePack.StopCapture;
var
  Seuil: ISeuil;
begin
  FFilter.QueryInterface(IID_ISeuil, Seuil);
  if Assigned(Seuil) then
    begin
      Seuil.SetActive(False);
      Seuil.SetCallback(nil);
    end;
  //(FGraph as IMediaControl).StopWhenReady;
  FIsCapturing := False;
  FGraph.ClearGraph;
  FGraph.Active := False;
end;

{ ================================================================================================ }
{ -------------------------------------- TCameraSynchronizer ------------------------------------- }
{ ================================================================================================ }

constructor TCameraSynchronizer.Create(AOwner: TCameraManager);
begin
  FOwner := AOwner;
  FPoints := T3DPointList.Create(0);
  FCaptureMode := cmNone;
  FPrevSyncTime := 0;
end;

destructor TCameraSynchronizer.Destroy;
begin
  FOwner := nil;
  FPoints.Clear;
  FreeAndNil(FPoints);
  inherited;
end;

procedure TCameraSynchronizer.DoOnCamera1GetPoints(Points: TListPoint);
begin
  FCamera1Points := Points;
  FCamera1Time := Time;

  DoSynchronize;
end;

procedure TCameraSynchronizer.DoOnCamera2GetPoints(Points: TListPoint);
begin
  FCamera2Points := Points;
  FCamera2Time := Time;

  DoSynchronize;
end;

procedure TCameraSynchronizer.DoSynchronize;
var
  IsSync: Boolean;
begin
  IsSync := False;
  case FCaptureMode of
    cmBoth:
      IsSync :=
        (FCamera1Time <> 0) and
        (FCamera2Time <> 0) and
        ((MillisecondsBetween(FCamera1LastTime, FCamera1Time) >= (Params['Interval'].AsInteger - 50)) or (FCamera1LastTime = 0)) and
        ((MillisecondsBetween(FCamera2LastTime, FCamera2Time) >= (Params['Interval'].AsInteger - 50)) or (FCamera2LastTime = 0));
    cmSingleFirst:
      IsSync :=
        (FCamera1Time <> 0) and
        ((MillisecondsBetween(FCamera1LastTime, FCamera1Time) >= (Params['Interval'].AsInteger - 50)) or (FCamera1LastTime = 0));
    cmSingleSecond:
      IsSync :=
        (FCamera2Time <> 0) and
        ((MillisecondsBetween(FCamera2LastTime, FCamera2Time) >= (Params['Interval'].AsInteger - 50)) or (FCamera2LastTime = 0));
    cmNone: // Should not be so...
      begin
        FOwner.StopCapture;
        raise ECameraManagerError.Create(cSynchronizeNoneModeError);
      end;
  end;

  if IsSync then
    begin
      Make3DList;
      FOwner.DoOnGetPoint(FPoints);
      FPrevSyncTime := Now;
      Drop;
    end;
end;

procedure TCameraSynchronizer.Drop;
begin
  FCamera1LastTime := FCamera1Time;
  FCamera2LastTime := FCamera2Time;
  FCamera1Time := 0;
  FCamera2Time := 0;
end;

function TCameraSynchronizer.GetOnCameraGetPoints(const Index: Byte): TOnLedDetectedCB;
begin
  Result := nil;
  case Index of
    0: Result := DoOnCamera1GetPoints;
    1: Result := DoOnCamera2GetPoints;
  end;
end;

procedure TCameraSynchronizer.Make3DList;

  procedure FillZeroPoints;
  var
    P: T3DPoint;
  var
    ToFill, I: Integer;
  begin
    ToFill := Params['PointCount'].AsInteger - FPoints.Count;
    P.X := 0;
    P.Y := 0;
    P.Z := 0;
    for I := 0 to ToFill - 1 do
      FPoints.Add(P);
  end;

var
  I, L: Integer;
  P: T3DPoint;
begin
  FPoints.Clear;
  FPoints.Time := MilliSecondsBetween(FPrevSyncTime, Now);
  case FCaptureMode of
    cmSingleFirst:
      for I := 0 to FCamera1Points.Count - 1 do
        begin
          P.X := FCamera1Points[I].X;
          P.Y := -1;
          P.Z := FCamera1Points[I].Y;
          FPoints.Add(P);
        end;
    cmSingleSecond:
      for I := 0 to FCamera2Points.Count - 1 do
        begin
          P.X := -1;
          P.Y := FCamera2Points[I].X;
          P.Z := FCamera2Points[I].Y;
          FPoints.Add(P);
        end;
    cmBoth:
      begin
        if FCamera1Points.Count > FCamera2Points.Count then
          L := FCamera2Points.Count
        else
          L := FCamera1Points.Count;
        for I := 0 to L do
          begin
            P.X := FCamera1Points[I].X;
            P.Y := FCamera2Points[I].X;
            P.Z := FCamera1Points[I].Y;
            FPoints.Add(P);
          end;
      end;
    cmNone: // Should not be so...
      begin
        FOwner.StopCapture;
        raise ECameraManagerError.Create(cSynchronizeNoneModeError);
      end;
  end;
  FillZeroPoints;
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
    FCams[I] := TCapturePack.Create(Self);
end;

destructor TCameraManager.Destroy;
begin
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

  Result.Add('Нет');
  for I := 0 to FCameraEnum.CountFilters - 1 do
    Result.Add(IntToStr(I) + ' - ' + FCameraEnum.Filters[I].FriendlyName);
end;

function TCameraManager.GetFirstCamera: TCapturePack;
begin
  Result := FCams[0];
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
  InitFilter;
  Res := FCams[0].Initialize(ACamera1, AFilter1, AGraph1, AVideoWindow1);
  Res := Res and FCams[1].Initialize(ACamera2, AFilter2, AGraph2, AVideoWindow2);
  if not Res then
    raise ECameraManagerError.Create(cFilterLoadError);
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

procedure TCameraManager.StartCapture;
var
  I: Integer;
  VideoWindow: IBaseFilter;
  AllowStart: Boolean;
begin
  if FCams[0].Active and FCams[1].Active then
    FCamSync.FCaptureMode := cmBoth
  else if FCams[0].Active then
    FCamSync.FCaptureMode := cmSingleFirst
  else if FCams[1].Active then
    FCamSync.FCaptureMode := cmSingleSecond
  else
    Exit;

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
  FCamSync.FPrevSyncTime := Now;
end;

procedure TCameraManager.StopCapture;
var
  I: Integer;
begin
  for I := 0 to 1 do
    FCams[I].StopCapture;
  DoOnStopCapture;
end;

initialization

  _CameraManager := TCameraManager.Create;

finalization

  FreeAndNil(_CameraManager);

end.
