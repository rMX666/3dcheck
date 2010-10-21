unit uDebug;

interface

uses
  SysUtils, Forms, Classes, Windows;

type
  TDebug = class
  private
    FOnException: TExceptionEvent;
    FFile: Integer;
    FFileName: String;
    FInstance:  THandle;
    FOldOnException: TExceptionEvent;
    procedure SetFileName(const Value: String);
    procedure SetException(const Value: TExceptionEvent);
    procedure DefaultExceptionHandler(Sender: TObject; E: Exception);
  protected
    function ExceptionToPChar(Sender: TObject; E: Exception): PChar;
  public
    constructor Create(HIstance: THandle);
    destructor Destroy; override;
    property FileName: String read FFileName write SetFileName;
    property OnException: TExceptionEvent read FOnException write SetException;
  end;

const
  DefauleDebugFileName = '.\Debug.log';

implementation

uses
  IniFiles;

{ TDebug }

constructor TDebug.Create(HIstance: THandle);
begin
  SetFileName(DefauleDebugFileName);
  FInstance := HInstance;
  FOldOnException := Application.OnException;
  Application.OnException := DefaultExceptionHandler;
end;

function TDebug.ExceptionToPChar(Sender: TObject; E: Exception): PChar;
var
  S, ObjectName, CallStack: String;
  ERec: SysUtils.PExceptionRecord;
const
  CSFormat = '[ %s ] ';
begin
  S := '[%s]'#13#10'Exception raised in module "%s" at [%s]'#13#10'On object: %s'#13#10'Exception message:'#13#10'%s' +
    #13#10'Call stack: %s'#13#10 + StringOfChar('-', 100) + #13#10#10#10;
  ObjectName := Sender.ClassName;
  if (Sender.InheritsFrom(TComponent)) then
    ObjectName := TComponent(Sender).Name + ' of type ' + ObjectName;
  if (E.InheritsFrom(EExternal)) then
    begin
      ERec := EExternal(E).ExceptionRecord;
      while ERec <> nil do
        begin
          CallStack := CallStack + Format(CSFormat, [IntToHex(Integer(ERec.ExceptionAddress), 8)]) + #13#10;
          ERec := ERec.ExceptionRecord;
        end;
    end;
  CallStack := '';
  Result := PChar(Format(S, [FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now), GetModuleName(FInstance),
    IntToHex(Integer(ExceptAddr), 8), ObjectName, E.ClassName + ': ' + E.Message, CallStack]));
end;

procedure TDebug.DefaultExceptionHandler(Sender: TObject; E: Exception);
var
  P: PChar;
begin
  if Assigned(FOldOnException) then
    FOldOnException(Sender, E);
  P := ExceptionToPChar(Sender, E);
  FileWrite(FFile, P^, Length(String(P)) - 1);
  MessageBox(Application.Handle, P, 'Œ¯Ë·Í‡', MB_OK or MB_ICONERROR);
end;

destructor TDebug.Destroy;
begin
  Application.OnException := nil;
  SysUtils.FileClose(FFile);
  inherited;
end;

procedure TDebug.SetException(const Value: TExceptionEvent);
begin
  FOnException := Value;
  Application.OnException := Value;
end;

procedure TDebug.SetFileName(const Value: String);
begin
  if FileName <> Value then
    begin
      FFileName := Value;
      SysUtils.FileClose(FFile);
      FFileName := ExpandFileName(FFileName);
      if not FileExists(FFileName) then
        begin
          FFile := SysUtils.FileCreate(FFileName);
          SysUtils.FileClose(FFile);
        end;
      FFile := SysUtils.FileOpen(FFileName, fmOpenWrite or fmShareDenyNone);
      SysUtils.FileSeek(FFile, 0, 2);
    end;
end;

var
  Debug: TDebug;

procedure InitDebug;
begin
  Debug := TDebug.Create(HInstance);
end;

procedure FreeDebug;
begin
  if Assigned(Debug) then
    FreeAndNil(Debug);
end;

initialization

  InitDebug;

finalization

  FreeDebug;

end.
