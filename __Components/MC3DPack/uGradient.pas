unit uGradient;

interface

uses Windows, Graphics, Controls, Classes, ExtCtrls, SysUtils, StdCtrls,
  Variants, Messages;

type
  TglPercent = 0..100;

  TglGradientDir = (fgdHorizontal, fgdVertical, fgdLeftBias, fgdRightBias, 
    fgdRectangle, fgdHorzConvergent, fgdVertConvergent);

  TglGlobalData = record
    fSuppressGradient: boolean;
    lp3DColors: Pointer;
  end;

type
  //********************************************************{ . TTwainColors . }
  TTwainColors = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    procedure SeTglomColor(const Value: TColor);
    procedure SetToColor(const Value: TColor);
  public
    FRGBFromColor: Longint;
    FRGBToColor: Longint;
    OnChanged: TNotifyEvent;
    constructor Create; virtual;
  published
    property FromColor: TColor read FFromColor write SeTglomColor default $00808080;
    property ToColor: TColor read FToColor write SetToColor default 0;
  end;

  //*****************************************************{ . TCustomGradient . }
  TCustomGradient = class(TTwainColors)
  public
    FOrientation: TglGradientDir; //...public!
    FActive: boolean;
  private
    FBufferedDraw: boolean;
    FSteps: integer;
    FPercentFilling: TglPercent;
    FBrushStyle: TBrushStyle;
    procedure SetActive(const Value: boolean);
    procedure SetOrientation(const Value: TglGradientDir);
    procedure SetSteps(Value: integer);
    procedure SetPercentFilling(const Value: TglPercent);
    procedure SetBrushStyle(const Value: TBrushStyle);
  protected
    property Active: boolean read FActive write SetActive;
    property BufferedDraw: boolean read FBufferedDraw write FBufferedDraw
      default false;
    property Orientation: TglGradientDir read FOrientation write SetOrientation;
    property Steps: integer read FSteps write SetSteps default 255;
    property PercentFilling: TglPercent read FPercentFilling write SetPercentFilling
      default 100;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle
      default bsSolid;
  public
    fReverse: boolean;
    procedure TextOut(DC: HDC; Str: string; TextR: TRect; x, y: integer);
    function GetColorFromGradientLine(GradientLineWidth, Position: word): COLORREF;
    constructor Create; override;
  end;

  //***********************************************************{ . TGradient . }
  TGradient = class(TCustomGradient)
  public
    procedure Draw(DC: HDC; r: TRect; PenStyle, PenWidth: integer);
  published
    property Active;
    property BufferedDraw;
    property Orientation;
    property Steps;
    property PercentFilling;
    property BrushStyle;
  end;
  
  //*******************************************************{ . TGradientPanel. }
  T3DGradientPanel = class(TPanel)
  private
    FGradient: TGradient;
    procedure OnSmthChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Gradient: TGradient read fGradient write fGradient;
  end;

  //************************************************************{ . TmyLabel . }
  TMyLabel = class(TLabel)
    public
      procedure AdjustHeight;
  end;
  
  //***************************************************************{ . TItem . }
  TItem = class(T3DGradientPanel)
  private
    FLabHead: TMyLabel;
    FLabBody: TMyLabel;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  end;
  //*******************************************************{ . TInfoListGrad . }
  TItm  = record
    Head: String;
    Body: String;
  end;

  TLab = record
    Head: TMyLabel;
    Body: TMyLabel;
  end;

  TArrayOfItem = array of TItm;
  TArrayOfLab = array of TLab;

  TInfoListGrad = class(T3DGradientPanel)
  private
    FHeadWidth: integer;  // Ширина заголовка элементов
    FHeadFont: TFont;     // Шрифт заголовка элементов
    FBodyFont: TFont;     // Шрифт тела элементов
    FTopOffSet: Integer;  // Отступ сверху
    FCount: integer;
    FArrayOfItem: TArrayOfItem;
    FArrayOfLab:  TArrayOfLab;
    procedure SetHeadWidth(Value: Integer);
    procedure SetHeadFont(Value: TFont);
    procedure SetBodyFont(Value: TFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(strHead, strBody: string);     // Добавить элемент
    procedure ShowItems;
    procedure Paint; override;
  published
    property HeadWidth: integer read FHeadWidth write SetHeadWidth;
    property FontHead: TFont read FHeadFont write SetHeadFont;
    property FontBody: TFont read FBodyFont write SetBodyFont;
    property TopOffSet: Integer read FTopOffSet write FTopOffSet;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MC3D', [T3DGradientPanel]);
end;

function Min(i1,i2: integer): integer;
begin
  if i1 < i2 then Result := i1 else Result := i2;
end;

//__________________________________________________{ . TTwainColors methods . }
constructor TTwainColors.Create;
begin
  inherited Create;
  //...set defaults
  FFromColor := $00808080;
  FRGBFromColor := ColorToRGB(FFromColor);
  FToColor := 0;
  FRGBToColor := ColorToRGB(FToColor);
end;

procedure TTwainColors.SeTglomColor(const Value: TColor);
begin
  if FFromColor = Value then Exit;
  FFromColor := Value;
  FRGBFromColor := ColorToRGB(Value);
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TTwainColors.SetToColor(const Value: TColor);
begin
  if FToColor = Value then Exit;
  FToColor := Value;
  FRGBToColor := ColorToRGB(Value);
  if Assigned(OnChanged) then OnChanged(self);
end;
//_______________________________________________{ . TCustomGradient methods . }
constructor TCustomGradient.Create;
begin
  inherited Create;
  //...set defaults
  FActive := false;
  FBufferedDraw := false;
  FOrientation := fgdHorizontal;
  FSteps := 255;
  FPercentFilling := 100;
  FBrushStyle := bsSolid;
end;

procedure TCustomGradient.SetActive(const Value: boolean);
begin
  if FActive = Value then exit;
  FActive := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetOrientation(const Value: TglGradientDir);
begin
  if FOrientation = Value then exit;
  FOrientation := Value;
  if FActive and Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetSteps(Value: integer);
begin
  if Value > 255 then Value := 255
    else if Value < 1 then Value := 1;
  if FSteps = Value then exit;
  FSteps := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetPercentFilling(const Value: TglPercent);
begin
  if FPercentFilling = Value then Exit;
  FPercentFilling := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

procedure TCustomGradient.SetBrushStyle(const Value: TBrushStyle);
begin
  FBrushStyle := Value;
  if Assigned(OnChanged) then OnChanged(self);
end;

function TCustomGradient.GetColorFromGradientLine(GradientLineWidth, 
  Position: word ): COLORREF;
var c1F,c2F,c3F: byte;
  c1T,c2T,c3T: byte;
  Step1,Step2,Step3: Single;
begin
  c1F := Byte(FRGBFromColor);
  c2F := Byte(WORD(FRGBFromColor) shr 8);
  c3F := Byte(FRGBFromColor shr 16);
  c1T := Byte(FRGBToColor);
  c2T := Byte(WORD(FRGBToColor) shr 8);
  c3T := Byte(FRGBToColor shr 16);

  Step1 := (c1T - c1F) / GradientLineWidth;
  Step2 := (c2T - c2F) / GradientLineWidth;
  Step3 := (c3T - c3F) / GradientLineWidth;

  Result := RGB
    (
      Trunc(c1F + Step1 * Position),
	    Trunc(c2F + Step2 * Position),
	    Trunc(c3F + Step3 * Position)
    );
end;

procedure TCustomGradient.TextOut(DC: HDC; Str: string; TextR: TRect; x, y: integer);
var i,Steps: integer;
  r: TRect;
  c1F,c2F,c3F: byte;
  c1T,c2T,c3T: byte;
  c1,c2,c3: Single;
  Step1,Step2,Step3: Single;
  OldTextColor: TCOLORREF;
begin
  if (not Active) or (GetDeviceCaps(DC, BITSPIXEL) < 16) then
    begin
      Windows.TextOut(DC, x, y, PChar(str), Length(str));
      Exit;
    end;
  r := TextR;
  c1F := Byte(FRGBFromColor);
  c2F := Byte(WORD(FRGBFromColor) shr 8);
  c3F := Byte(FRGBFromColor shr 16);
  c1T := Byte(FRGBToColor);
  c2T := Byte(WORD(FRGBToColor) shr 8);
  c3T := Byte(FRGBToColor shr 16);

  c1 := c1F;
  c2 := c2F;
  c3 := c3F;
  if FOrientation = fgdVertical then
    Steps := r.right - r.left
  else Steps := r.bottom - r.top;
  Step1 := (c1T - c1F) / Steps;
  Step2 := (c2T - c2F) / Steps;
  Step3 := (c3T - c3F) / Steps;

  OldTextColor := SetTextColor(DC, 0);
  Steps := MulDiv(Steps, PercentFilling, 100);
  for i:=0 to Steps do
    begin
      SetTextColor( DC, RGB( trunc(c1),trunc(c2),trunc(c3)) );
      if FOrientation = fgdVertical then
        begin
          r.left := i;
          r.right := r.left + 1;
        end
      else
        begin
          r.top := i;
          r.bottom := r.top + 1;
        end;
      Windows.ExtTextOut(DC, x, y,  ETO_CLIPPED, @r, PChar(str), Length(str), nil);
      c1 := c1 + Step1;
      c2 := c2 + Step2;
      c3 := c3 + Step3;
    end;
  SetTextColor(DC, OldTextColor);
end;

//_____________________________________________________{ . TGradient methods . }
procedure TGradient.Draw(DC: HDC; r: TRect; PenStyle, PenWidth: integer);
var i, j, x, y, x2, y2, h, w, NumberOfColors: integer;
  c1F, c2F, c3F: byte;
  c1T, c2T, c3T: byte;
  c1D, c2D, c3D: integer;
  _R, _G, _B: byte;
  Pen, OldPen: HPen;
  FillBrush: HBrush;
  BufferBmp, OldBMP: HBITMAP;
  BufferDC, TargetDC: HDC;
  ColorR: TRect;
  LOGBRUSH: TLOGBRUSH;

  procedure SwapColors;
  var TempColor: Longint;
  begin
    TempColor := FRGBFromColor;
    FRGBFromColor := FRGBToColor;
    FRGBToColor := TempColor;
  end;
  
begin
  if not Active then Exit;
  if (Steps = 1) or (GetDeviceCaps(DC, BITSPIXEL) < 16) then
    begin
{
    FillBrush := CreateSolidBrush(ColorToRGB(FromColor));
    FillRect(DC, r, FillBrush );
    DeleteObject(FillBrush );
//}
      Exit;
    end;
  x := r.left;
  y := r.top;
  h := r.bottom - r.top;
  w := r.right - r.left;
  x2 := 0;
  y2 := 0;
  pen := 0;
  oldpen := 0;
  BufferDC := 0;

  if Orientation = fgdHorzConvergent then
    begin
      FOrientation := fgdHorizontal;
      Draw(DC, Rect(R.Left, R.Top, R.Right, R.Bottom - h div 2), PenStyle, PenWidth);
      SwapColors;
      Draw(DC, Rect(R.Left, R.Top + h div 2, R.Right, R.Bottom), PenStyle, PenWidth);
      SwapColors;
      FOrientation := fgdHorzConvergent;
      Exit;
    end;
  if Orientation = fgdVertConvergent then
    begin
      FOrientation := fgdVertical;
      Draw(DC, Rect(R.Left, R.Top, R.Right - w div 2, R.Bottom), PenStyle, PenWidth);
      SwapColors;
      Draw(DC, Rect(R.Left + w div 2, R.Top, R.Right, R.Bottom), PenStyle, PenWidth);
      SwapColors;
      FOrientation := fgdVertConvergent;
      Exit;
    end;

  //...r._ data no more useful
  c1F := Byte(FRGBFromColor);
  c2F := Byte(WORD(FRGBFromColor) shr 8);
  c3F := Byte(FRGBFromColor shr 16);
  c1T := Byte(FRGBToColor);
  c2T := Byte(WORD(FRGBToColor) shr 8);
  c3T := Byte(FRGBToColor shr 16);
  c1D := c1T - c1F;
  c2D := c2T - c2F;
  c3D := c3T - c3F;

  if BufferedDraw then
    begin
      BufferDC := CreateCompatibleDC(DC);
      BufferBmp := CreateBitmap(w, h, GetDeviceCaps(DC, PLANES), GetDeviceCaps(DC, BITSPIXEL), nil);
      OldBMP := SelectObject(BufferDC, BufferBmp);
      SetMapMode(BufferDC, GetMapMode(DC));
      TargetDC := BufferDC;
    end
  else TargetDC := DC;

  case Orientation of
    fgdHorizontal:
      begin
        NumberOfColors := min( Steps, h );
        ColorR.Left := r.left; ColorR.Right  := r.right;
      end;
    fgdVertical:
      begin
        NumberOfColors := min( Steps, w );
        ColorR.Top := r.top; ColorR.Bottom  := r.bottom;
      end;
    fgdLeftBias, fgdRightBias:
      begin
        NumberOfColors := min(Steps, w+h);
        if PenStyle = 0 then PenStyle := PS_SOLID;
        if PenWidth = 0 then PenWidth := 1;
        Pen := CreatePen(PenStyle, PenWidth, 0);
        OldPen := SelectObject(TargetDC, Pen);
        y2 := y;
        if Orientation = fgdLeftBias then
          x2 := x
        else
		      begin
            x := r.right;
            x2 := r.right;
          end;
      end;
    else{fgdRectangle}
      begin
        h:=h div 2;
        w:=w div 2;
        NumberOfColors := min(Steps, min(w,h));
      end;
  end;
  LOGBRUSH.lbStyle := BS_HATCHED;
  LOGBRUSH.lbHatch := Ord(BrushStyle) - Ord(bsHorizontal);
  for i := 0 to NumberOfColors - 1 do
    begin
      _R := c1F + MulDiv(i, c1D, NumberOfColors - 1);
      _G := c2F + MulDiv(i, c2D, NumberOfColors - 1);
      _B := c3F + MulDiv(i, c3D, NumberOfColors - 1);
    case Orientation of
      fgdHorizontal, fgdVertical, fgdRectangle:
        begin
          if BrushStyle = bsSOLID then
            FillBrush := CreateSolidBrush(RGB( _R, _G, _B ))
          else
            begin
              LOGBRUSH.lbColor := RGB( _R, _G, _B );
              FillBrush := CreateBrushIndirect(LOGBRUSH);
            end;

          case Orientation of
            fgdHorizontal:
              begin
                if fReverse then
                  begin
                    ColorR.Top := r.bottom - MulDiv(i, h, NumberOfColors);
                    ColorR.Bottom := r.bottom - MulDiv(i + 1, h, NumberOfColors);
                  end
                else
                  begin
                    ColorR.Top := r.top + MulDiv(i, h, NumberOfColors);
                    ColorR.Bottom := r.top + MulDiv(i + 1, h, NumberOfColors);
                  end;
              end;
            fgdVertical:
              begin
                if fReverse then
                  begin
                    ColorR.Left := r.right - MulDiv(i, w, NumberOfColors);
                    ColorR.Right := r.right - MulDiv(i + 1, w, NumberOfColors);
                  end
                else
                  begin
                    ColorR.Left := r.left + MulDiv(i, w, NumberOfColors);
                    ColorR.Right := r.left + MulDiv(i + 1, w, NumberOfColors);
                  end;
              end;
            fgdRectangle:
              begin
                ColorR.Top	:= r.top + MulDiv(i, h, NumberOfColors);
                ColorR.Bottom := r.bottom - MulDiv(i, h, NumberOfColors);
                ColorR.Left	:= r.left + MulDiv(i, w, NumberOfColors);
                ColorR.Right := r.right - MulDiv(i, w, NumberOfColors);
              end;
          end;
          FillRect(TargetDC, ColorR, FillBrush);
          DeleteObject(FillBrush);
        end;
    else {fgdLeftBias, fgdRightBias:}
      begin
        if Pen <> 0 then
          DeleteObject(SelectObject(TargetDC, OldPen));//...cant delete selected!
        Pen := CreatePen(PenStyle, PenWidth, RGB(_R, _G, _B ));
        OldPen := SelectObject(TargetDC, Pen);
        for j := 1 to MulDiv(i + 1, h + w, NumberOfColors) - MulDiv(i , h + w, NumberOfColors) do
          begin
            case Orientation of
              fgdLeftBias:
                begin
                  if y >= r.bottom then
                    inc(x, PenWidth)
                  else y := y + PenWidth;
                  if x2 >= r.right then
                    inc(y2, PenWidth)
                  else x2 := x2 + PenWidth;
                  MoveToEx(TargetDC, x, y, nil);
                  LineTo(TargetDC, x2, y2);
                end;
              else{fgdRightBias:}
                begin
                  if x <= r.left then inc(y, PenWidth) else x := x - PenWidth;
                  if y2 >= r.bottom then dec(x2, PenWidth) else y2 := y2 + PenWidth;
                  MoveToEx(TargetDC, x, y, nil);
                  LineTo(TargetDC, x2, y2);
                end;
            end;{end case}
          end;{end for}
        DeleteObject(SelectObject(TargetDC, OldPen));
      end;{end case else}
    end;{end case}
//    if NumberOfColors=0 then exit;
    if i / NumberOfColors * 100 > PercentFilling then break;
  end;{end for}

  if BufferedDraw then
    begin
      BitBlt(DC, 0, 0, r.right-r.left, r.bottom-r.top, BufferDC, 0, 0, SRCCOPY);
      DeleteObject(SelectObject(BufferDC, OldBMP));
      DeleteDC(BufferDC);
    end;
end;

//________________________________________________{ . TGradientPanel methods . }
constructor T3DGradientPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGradient := TGradient.Create;
  FGradient.OnChanged := OnSmthChanged;
end;

procedure T3DGradientPanel.Paint;
var r: TRect;
begin
  inherited;
  r := ClientRect;
  FGradient.Draw(Canvas.Handle, r, 0, 1);
end;

procedure T3DGradientPanel.OnSmthChanged(Sender: TObject);
begin
  Repaint;
end;

//______________________________________________________{ . TMyLabel methods . }
procedure TMyLabel.AdjustHeight();
const WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var text: string;
  Rect: TRect;
  DC: HDC;
  Flags: Longint;
begin
  Rect := ClientRect;
  DC := GetDC(0);
  Canvas.Handle := DC;
  Flags := (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap];
  Text := GetLabelText;
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
  Canvas.Handle := 0;
  ReleaseDC(0, DC);
  Height := Rect.Bottom;
end;

//_________________________________________________________{ . TItem methods . }
constructor TItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLabHead := TMyLabel.Create(Self);
  FLabHead.Parent := Self;
  FLabHead.Transparent := True;
  FLabHead.WordWrap := True;
  FLabBody := TMyLabel.Create(Self);
  FLabBody.Parent := Self;
  FLabBody.Transparent := True;
  FLabBody.WordWrap := True;
  Self.BevelOuter := bvNone;
end;

destructor TItem.Destroy;
begin
  FLabHead.Free;
  FLabBody.Free;
  inherited Destroy;
end;

procedure TItem.Paint;
begin
  inherited;
  //self.FLabHead.Paint;
  //self.FLabBody.Paint;
end;

//___________________________________________________{ . TInfoListGrad metods. }
constructor TInfoListGrad.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeadFont := TFont.Create;
  FBodyFont := TFont.Create;
  FHeadWidth := 100;
  Self.BevelOuter := bvNone;
  Self.Caption := '';
  FCount := 0;
  SetLength(FArrayOfItem, FCount);
  SetLength(FArrayOfLab,  FCount);
end;

destructor TInfoListGrad.Destroy;
begin
  FHeadFont.Free;
  FBodyFont.Free;
  inherited Destroy;
end;

procedure TInfoListGrad.AddItem(strHead, strBody: string);
begin
  FCount := FCount + 1;
  SetLength (FArrayOfItem, FCount);
  SetLength (FArrayOfLab,  FCount);
  FArrayOfItem[FCount-1].Head := strHead;
  FArrayOfItem[FCount-1].Body := strBody;
end;

procedure TInfoListGrad.ShowItems;
var i: integer;
begin
  self.DestroyComponents;
  //SetLength (Items, FCount);
  for i := 0 to FCount-1 do
    begin
      FArrayOfLab[i].Head := TMyLabel.Create(Self);
      FArrayOfLab[i].Body := TMyLabel.Create(Self);
      //Items[i].Color := Self.Color;
      //Items[i].Gradient:= self.Gradient;
      //Items[i].Parent := Self;

      ////Item.Align := AlTop;
      FArrayOfLab[i].Head.Parent := Self;
      FArrayOfLab[i].Head.Caption := FArrayOfItem[i].Head;
      FArrayOfLab[i].Head.Font := FHeadFont;
      FArrayOfLab[i].Head.Alignment := taRightJustify;
      FArrayOfLab[i].Head.Transparent := True;
      FArrayOfLab[i].Head.WordWrap := True;

      FArrayOfLab[i].Body.Parent := Self;
      FArrayOfLab[i].Body.Caption := FArrayOfItem[i].Body;
      FArrayOfLab[i].Body.Font := FBodyFont;
      FArrayOfLab[i].Body.Left := FHeadWidth+5;
      FArrayOfLab[i].Body.Transparent := True;
      FArrayOfLab[i].Body.WordWrap := True;
    end;
end;

procedure TInfoListGrad.SetHeadWidth(value: integer);
begin
  FHeadWidth := Value;
end;

procedure TInfoListGrad.SetHeadFont(value: TFont);
begin
  FHeadFont.Assign(Value);
end;

procedure TInfoListGrad.SetBodyFont(value: TFont);
begin
  FBodyFont.Assign(Value);
end;

procedure TInfoListGrad.Paint;
var i, H: Integer;
begin
  inherited;
  for i := 0 to FCount - 1 do
    with FArrayOfLab[i] do
      begin
        Body.Width := Self.Width - FHeadWidth - 10; // Устанавливается ширина тела-элемета
        Head.Width := Self.HeadWidth;
        Body.AdjustHeight;
        Head.AdjustHeight;
        if i = 0 then
          begin
            Head.Top := FTopOffSet;
            Body.Top := FTopOffSet;
          end
        else
          begin
            if FArrayOfLab[i - 1].Body.Height > FArrayOfLab[i - 1].Head.Height then
              H := FArrayOfLab[i - 1].Body.Height + 10
            else H := FArrayOfLab[i - 1].Head.Height + 10;
            Head.Top := FArrayOfLab[i - 1].Head.Top + H;
            Body.Top := FArrayOfLab[i - 1].Body.Top + H;
          end;
      end;
end;

end.
