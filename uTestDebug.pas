unit uTestDebug;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  uMCPoint;

type
  TfTestDebug = class(TForm)
    mDebug: TMemo;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
  public
    procedure Add(const S: String); overload;
    procedure Add(const P: T3DPointList); overload;
    function IsActive: Boolean;
  end;

var
  fTestDebug: TfTestDebug;

implementation

const
  cStringLogFormat = '[ %s ] %s';
  cPointLogFormat = '  [ %d ] Time: %d, X: %f, Y: %f, Z: %f';

{$R *.dfm}

procedure TfTestDebug.Add(const S: String);
begin
  if IsActive then
    mDebug.Lines.Add(Format(cStringLogFormat, [FormatDateTime('hh:nn:ss.zzz', Now), S]));
end;

procedure TfTestDebug.Add(const P: T3DPointList);
var
  I: Integer;
begin
  if IsActive then
    begin
      Add('Points ---------------------------------');
      for I := 0 to P.Count - 1 do
        mDebug.Lines.Add(Format(cPointLogFormat, [(I + 1), P.Time, P[I].X, P[I].Y, P[I].Z]));
      Add('Points ---------------------------------');
    end;
end;

procedure TfTestDebug.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfTestDebug.FormShow(Sender: TObject);
begin
  Left := Screen.Width - Width;
  Top := 300;
end;

function TfTestDebug.IsActive: Boolean;
begin
  Result := Visible;
end;

end.
