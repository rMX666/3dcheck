unit uConfirmDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFConfirmDialog = class(TForm)
    lblConfirmation: TLabel;
    Image1: TImage;
    btnReplace: TButton;
    btnSkip: TButton;
    btnCancel: TButton;
    btnReplaceAll: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    procedure PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer; AReplaceText: string);
  end;

var
  FConfirmDialog: TFConfirmDialog;

resourcestring
  SAskReplaceText = 'Заменить это вхождение на %s?';

implementation

{$R *.dfm}

{ TForm2 }

procedure TFConfirmDialog.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TFConfirmDialog.FormDestroy(Sender: TObject);
begin
  Self := nil;
end;

procedure TFConfirmDialog.PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer;
  AReplaceText: string);
var nW, nH: integer;
begin
  lblConfirmation.Caption := Format(SAskReplaceText, [AReplaceText]);
  nW := AEditorRect.Right - AEditorRect.Left;
  nH := AEditorRect.Bottom - AEditorRect.Top;

  if nW <= Width then
    X := AEditorRect.Left - (Width - nW) div 2
  else
    begin
      if X + Width > AEditorRect.Right then
        X := AEditorRect.Right - Width;
    end;
  if Y2 > AEditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

end.
