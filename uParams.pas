unit uParams;

interface

uses
  SysUtils, Classes, IniFiles;

type
  TParams = class;

  TParam = class
  private
    FOwner: TParams;
    FName: String;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsFloat: Real;
    function GetAsInteger: Integer;
    function GetAsString: String;
    procedure SetAsFloat(const Value: Real);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
  public
    constructor Create(AOwner: TParams; AName: String);
    property Name: String read FName write FName;
    property AsFloat: Real read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: String read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
  end;

  TParams = class
  private
    FIni: TIniFile;
    FParams: TStrings;
    FSaveOnDestroy: Boolean;
    FSaveOnWrite: Boolean;
    function GetCount: Integer;

    function GetParam(const Name: String): TParam;
  public
    constructor Create(AIniName: String; const ReadOnCreate: Boolean = True);
    destructor Destroy; override;
    procedure Clear;
    procedure ReadParams;
    procedure SaveParams;
    function ParamByIndex(const Index: Integer): TParam;
    property Count: Integer read GetCount;
    property Params[const Name: String]: TParam read GetParam; default;
    property SaveOnDestroy: Boolean read FSaveOnDestroy write FSaveOnDestroy;
    property SaveOnWrite: Boolean read FSaveOnWrite write FSaveOnWrite;
  end;

function Params: TParams;

implementation

const
  DefaultParamsFile = '.\Params.ini';

var
  _Params: TParams;

function Params: TParams;
begin
  Result := _Params;
end;

{ TParam }

constructor TParam.Create(AOwner: TParams; AName: String);
begin
  FOwner := AOwner;
  FName := AName;
end;

function TParam.GetAsBoolean: Boolean;
begin
  Result := AnsiLowerCase(FOwner.FParams.Values[FName]) = 'true';
end;

function TParam.GetAsFloat: Real;
var
  S: String;
  res: Extended;
begin
  if not TryStrToFloat(FOwner.FParams.Values[FName], res) then
    begin
      S := StringReplace(FOwner.FParams.Values[FName], '.', ',', [rfReplaceAll]);
      if not TryStrToFloat(S, Extended(res)) then
        res := 0;
    end;
  Result := res;
end;

function TParam.GetAsInteger: Integer;
begin
  Result := StrToIntDef(FOwner.FParams.Values[FName], 0);
end;

function TParam.GetAsString: String;
begin
  Result := FOwner.FParams.Values[FName];
end;

procedure TParam.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    FOwner.FParams.Values[FName] := 'True'
  else
    FOwner.FParams.Values[FName] := 'False';
  if FOwner.FSaveOnWrite then
    FOwner.FIni.WriteBool('Configuration', FName, Value);
end;

procedure TParam.SetAsFloat(const Value: Real);
begin
  FOwner.FParams.Values[FName] := FloatToStr(Value);
  if FOwner.FSaveOnWrite then
    FOwner.FIni.WriteFloat('Configuration', FName, Value);
end;

procedure TParam.SetAsInteger(const Value: Integer);
begin
  FOwner.FParams.Values[FName] := IntToStr(Value);
  if FOwner.FSaveOnWrite then
    FOwner.FIni.WriteInteger('Configuration', FName, Value);
end;

procedure TParam.SetAsString(const Value: String);
begin
  FOwner.FParams.Values[FName] := Value;
  if FOwner.FSaveOnWrite then
    FOwner.FIni.WriteString('Configuration', FName, Value);
end;

{ ------------------------------------------- TParams ------------------------------------------- }

procedure TParams.Clear;
var
  I: Integer;
begin
  for I := 0 to FParams.Count - 1 do
    FParams.Objects[I].Free;
  FParams.Clear;
end;

constructor TParams.Create(AIniName: String; const ReadOnCreate: Boolean = True);
begin
  FIni := TIniFile.Create(AIniName);
  FSaveOnDestroy := True;
  FSaveOnWrite := False;
  FParams := TStringList.Create;
  if ReadOnCreate then
    ReadParams;
  FSaveOnWrite := True;
end;

destructor TParams.Destroy;
begin
  if FSaveOnDestroy then
    SaveParams;
  FreeAndNil(FIni);
  Clear;
  FreeAndNil(FParams);
  inherited;
end;

function TParams.GetCount: Integer;
begin
  Result := FParams.Count;
end;

function TParams.GetParam(const Name: String): TParam;
begin
  if FParams.IndexOfName(Name) = -1 then
    begin
      Result := TParam.Create(Self, Name);
      FParams.AddObject(Name + '=', Result);
    end
  else
    Result := TParam(FParams.Objects[FParams.IndexOfName(Name)]);
end;

function TParams.ParamByIndex(const Index: Integer): TParam;
begin
  Result := TParam(FParams.Objects[Index]);
end;

procedure TParams.ReadParams;
var
  Section: TStrings;
  I: Integer;
begin
  Section := TStringList.Create;
  try
    FIni.ReadSection('Configuration', Section);
    for I := 0 to Section.Count - 1 do
      Self[Section[I]].AsString := FIni.ReadString('Configuration', Section[I], '');
  finally
    FreeAndNil(Section);
  end;
end;

procedure TParams.SaveParams;
var
  I: Integer;
begin
  for I := 0 to FParams.Count - 1 do
    FIni.WriteString('Configuration', FParams.Names[I], FParams.ValueFromIndex[I]);
end;

initialization

  _Params := TParams.Create(ExpandFileName(DefaultParamsFile));

finalization

  FreeAndNil(_Params);

end.
