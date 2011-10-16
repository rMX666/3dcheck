unit SeuillageProcessor_RGB24;

interface

uses
  BaseClass, DirectShow9, DSUtil, ActiveX, Windows, Math, Types,
  Seuillage_inc, SeuillageProcessor, SysUtils;

type
  TSeuillageProcessor_RGB24 = class (TSeuillageProcessor)
  protected
    Mask : TMaskSeuil;
    ZeroMask: TMaskSeuil;
    procedure DrawCenterCross; override;
    procedure DrawGrid; override;
    procedure DrawCross(aPoint : TPoint); override;
    procedure Find(pPixel : Pointer); override;
    function PointCenterWeightedMean: TPoint; override;
    function TestPixel(pPixel: Pointer): Boolean; override;
  public
    function SetSeuil(bSeuil: Byte): HResult; override;
    procedure Threshold(pData : pByte); override;
  end;
  

{$i FreetrackFilter.inc}

implementation

const
  X_POINT_COL = $80;
  X_CENTER_COL = $50;

{ TSeuillageProcessor_RGB24 }

{
************************** TSeuillageProcessor_RGB24 ***************************
}
procedure TSeuillageProcessor_RGB24.DrawCenterCross;
var
  pPixelVert, pPixelHor: PRGBTriple;
  i: Integer;
  iPixelSizeXSampleWidth: Integer;
begin
  pPixelHor := PRGBTriple(FAddrStart + iPixelSize*(round(0.5 * SampleHeight)) * SampleWidth);
  pPixelVert := PRGBTriple(FAddrStart + iPixelSize*(round(0.5 * SampleWidth)));
  iPixelSizeXSampleWidth := iPixelSize*SampleWidth;
  
  for i := 0 to SampleWidth do begin
    if (Longint(pPixelHor) >= FAddrStart) and (Longint(pPixelHor) <= FAddrEnd) then
      FillChar(pPixelHor^, iPixelSize, X_CENTER_COL);
    if (Longint(pPixelVert) >= FAddrStart) and (Longint(pPixelVert) <= FAddrEnd) then
      FillChar(pPixelVert^, iPixelSize, X_CENTER_COL);
  
    pPixelHor  := PRGBTriple(Longint(pPixelHor) + iPixelSize);
    pPixelVert := PRGBTriple(Longint(pPixelVert) + iPixelSizeXSampleWidth);
  end;
end;

procedure TSeuillageProcessor_RGB24.DrawGrid;
var
  pPixelVert, pPixelHor: array of PRGBTriple;
  i, j: Integer;
  iPixelSizeXSampleWidth: Integer;
begin
  SetLength(pPixelHor, GridSize);
  SetLength(pPixelVert, GridSize);
  for j := 0 to GridSize - 1 do
    begin
      pPixelHor[j] := PRGBTriple(FAddrStart + iPixelSize*(round((1 / GridSize) * j * SampleHeight)) * SampleWidth);
      pPixelVert[j] := PRGBTriple(FAddrStart + iPixelSize*(round((1 / GridSize) * j * SampleWidth)));
    end;
  iPixelSizeXSampleWidth := iPixelSize*SampleWidth;
  
  for i := 0 to SampleWidth do
    for j := 0 to GridSize - 1 do
      begin
        if (Longint(pPixelHor[j]) >= FAddrStart) and (Longint(pPixelHor[j]) <= FAddrEnd) then
          FillChar((pPixelHor[j])^, iPixelSize, X_CENTER_COL);
        if (Longint(pPixelVert[j]) >= FAddrStart) and (Longint(pPixelVert[j]) <= FAddrEnd) then
          FillChar((pPixelVert[j])^, iPixelSize, X_CENTER_COL);

        pPixelHor[j]  := PRGBTriple(Longint(pPixelHor[j]) + iPixelSize);
        pPixelVert[j] := PRGBTriple(Longint(pPixelVert[j]) + iPixelSizeXSampleWidth);
      end;

  SetLength(pPixelHor, 0);
  SetLength(pPixelVert, 0);
end;

procedure TSeuillageProcessor_RGB24.DrawCross(aPoint : TPoint);
var
  pPixelLeft, pPixelRight, pPixelUp, pPixelDown : PRGBTriple;
  i : integer;
  iPixelSizeXSampleWidth : integer;
begin
  pPixelLeft := PRGBTriple(AddrStart + iPixelSize*(aPoint.X + (SampleHeight - aPoint.Y) * SampleWidth));
  pPixelRight := pPixelLeft;
  pPixelUp  := pPixelLeft;
  pPixelDown := pPixelLeft;
  iPixelSizeXSampleWidth := iPixelSize*SampleWidth;

  for i := 0 to X_POINT_SIZE do begin
    if (aPoint.Y - i) > 0 then
      FillChar(pPixelUp^, iPixelSize, X_POINT_COL);
    if (aPoint.X - i) > 0 then
      FillChar(pPixelLeft^, iPixelSize, X_POINT_COL);

    if (aPoint.Y + i) < SampleHeight then
      FillChar(pPixelDown^, iPixelSize, X_POINT_COL);
    if (aPoint.X + i)  < SampleWidth then
      FillChar(pPixelRight^, iPixelSize, X_POINT_COL);

    pPixelRight := PRGBTriple(Longint(pPixelRight) + iPixelSize);
    pPixelLeft := PRGBTriple(Longint(pPixelLeft) - iPixelSize);
    pPixelUp := PRGBTriple(Longint(pPixelUp) + iPixelSizeXSampleWidth);
    pPixelDown := PRGBTriple(Longint(pPixelDown) - iPixelSizeXSampleWidth);
  end;
end;

procedure TSeuillageProcessor_RGB24.Find(pPixel : Pointer);
var
  x, y : Integer;
  iPixelSizeXSampleWidth, aPixAddr: Integer;
begin
  if  (aLed.Bottom - aLed.Top > POINT_MAX_DIM) or
      (aLed.Right - aLed.Left > POINT_MAX_DIM) then
    Exit;

  try
    if (PRGBTriple(pPixel).rgbtRed = PIX_LIGHT) then begin
      PRGBTriple(pPixel).rgbtRed := PIX_USED; // set the pixel as used

      asm
        mov esi, Self
        mov eax, Longint(pPixel)
        sub eax, [esi].TSeuillageProcessor_RGB24.FAddrStart
        xor edx, edx
        mov ecx, 3
        div ecx
        div [esi].TSeuillageProcessor_RGB24.SampleWidth
        mov x, edx
        //x := ((Longint(pPixel) - FAddrStart)div iPixelSize) mod SampleWidth;
  
        mov ecx, [esi].TSeuillageProcessor_RGB24.SampleHeight
        sub ecx, eax
        mov y, ecx
        //y := SampleHeight - ((Longint(pPixel) - FAddrStart)div iPixelSize) div SampleWidth;
      end;

      aLed := Rect(Min(aLed.Left, x), Min(aLed.Top, y), Max(aLed.Right, x), Max(aLed.Bottom, y));

      iPixelSizeXSampleWidth := iPixelSize*SampleWidth;

      aPixAddr := Longint(pPixel) - iPixelSizeXSampleWidth - iPixelSize;
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x-1,y+1
      inc(aPixAddr, iPixelSize);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x,y-1
      inc(aPixAddr, iPixelSize);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x+1,y-1
  
      inc(aPixAddr, iPixelSizeXSampleWidth - iPixelSize shl 1);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x-1,y
      inc(aPixAddr, iPixelSize shl 1);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x+1,y
  
      inc(aPixAddr, iPixelSizeXSampleWidth - iPixelSize shl 1);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x-1,y-1
      inc(aPixAddr, iPixelSize);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x,y+1
      inc(aPixAddr, iPixelSize);
      if inside(aPixAddr) then Find(PRGBTriple(aPixAddr));        //x+1,y+1

    end;
  
  except
    On E : Exception do
      {  Raise ;
      else}
      Raise Exception.Create('Error in TfmGrabber.Find' + E.Message);
  end;
end;

function TSeuillageProcessor_RGB24.PointCenterWeightedMean: TPoint;
var
  jPixel, xPixel, yPixel, iPixelSizeXSampleWidth: Integer;
  pixelCount, average_x, average_y: Integer;
  ledXWeight, ledYWeight: array[0..200] of Integer;
  aPixel: PRGBTriple;
  startPixel: LongInt;
begin
  
  pixelCount := 0;
  average_x := 0;
  average_y := 0;
  ZeroMemory(@ledXWeight, SizeOf(ledXWeight));
  ZeroMemory(@ledYWeight, SizeOf(ledXWeight));
  
  iPixelSizeXSampleWidth := iPixelSize * SampleWidth;
  
  // RGB Y axis is inverted
  // range limits used to avoid integer overflow
  startPixel := AddrStart + iPixelSize * (aLed.Left + (SampleHeight - aLed.Top) * SampleWidth);
  for xPixel := 0 to Min(aLed.Right - aLed.Left, MaxPointSize)  do
    for yPixel := 0 to Min(aLed.Bottom - aLed.Top, MaxPointSize) do begin
      aPixel := PRGBTriple(startPixel + iPixelSize * xPixel - iPixelSizeXSampleWidth * yPixel);
      if aPixel.rgbtRed = PIX_USED then begin
        aPixel.rgbtRed := PIX_TRACKED;
        Inc(ledXWeight[xPixel]);
        Inc(ledYWeight[yPixel]);
        Inc(pixelCount);
      end;
    end;

  for jPixel := 0 to Min(Max(aLed.Right - aLed.Left, aLed.Bottom - aLed.Top), MaxPointSize) do begin
    average_x := average_x + ledXWeight[jPixel] * (jPixel + 1);
    average_y := average_y + ledYWeight[jPixel] * (jPixel + 1);
  end;
  
  average_x := round((aLed.Left + (average_x / pixelCount) - 1) * LISTPOINT_SCALER);
  average_y := round((aLed.Top + (average_y / pixelCount) - 1) * LISTPOINT_SCALER);
  
  Result := Point(average_x, average_y);
end;

function TSeuillageProcessor_RGB24.SetSeuil(bSeuil: Byte): HResult;
begin
  inherited SetSeuil(bSeuil);

  FillChar(GreyMask, SizeOf(TMaskSeuil), PIX_LIGHT);
  {$ifdef SSE2}
  //15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
  // B  R  G  B  R  G  B  R  G  B  R  G  B  R  G  B
  Mask1[2] := bSeuil - $80;
  Mask1[5] := bSeuil - $80;
  Mask1[8] := bSeuil - $80;
  Mask1[11] := bSeuil - $80;
  Mask1[14] := bSeuil - $80;

  //15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
  // G  B  R  G  B  R  G  B  R  G  B  R  G  B  R  G
  Mask2[1] := bSeuil - $80;
  Mask2[4] := bSeuil - $80;
  Mask2[7] := bSeuil - $80;
  Mask2[10] := bSeuil - $80;
  Mask2[13] := bSeuil - $80;
  
  //15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
  // R  G  B  R  G  B  R  G  B  R  G  B  R  G  B  R
  Mask3[0] := bSeuil - $80;
  Mask3[3] := bSeuil - $80;
  Mask3[6] := bSeuil - $80;
  Mask3[9] := bSeuil - $80;
  Mask3[12] := bSeuil - $80;
  Mask3[15] := bSeuil - $80;

  {$else}
  Mask1[2] := bSeuil - $80;
  Mask1[5] := bSeuil - $80;

  // B  R  G  B   R  G  B  R
  Mask2[0] := bSeuil - $80;
  Mask2[3] := bSeuil - $80;
  Mask2[6] := bSeuil - $80;

  // R  G  B  R   G  B  R  G
  Mask3[1] := bSeuil - $80;
  Mask3[4] := bSeuil - $80;
  Mask3[7] := bSeuil - $80;
  {$endif}
  
  Result := S_OK;
end;

function TSeuillageProcessor_RGB24.TestPixel(pPixel: Pointer): Boolean;
begin
  Result := RGBTRIPLE(pPixel^).rgbtRed = PIX_LIGHT;
end;

procedure TSeuillageProcessor_RGB24.Threshold(pData : pByte);
begin
  inherited;

  asm
    {$ifdef SSE2}
    mov eax, pData                            //eax point on first image bit
    sub eax, MM_SIZE                          //16 = sizeof xmm0
    mov ebx, self

    mov ecx, [ebx].TSeuillageProcessor.SampleSize      //1 loop updates 48 bytes
    movdqu xmm3, SignOffset                   //offset pour la conversion non-sign�/sign�
    movdqu xmm4, [ebx].TSeuillageProcessor.GreyMask

    movdqu xmm7, [ebx].TSeuillageProcessor.Mask3       //fixe le seuil sur 8 octects ds xmm1
    movdqu xmm6, [ebx].TSeuillageProcessor.Mask2       //fixe le seuil sur 8 octects ds xmm1
    movdqu xmm5, [ebx].TSeuillageProcessor.Mask1       //fixe le seuil sur 8 octects ds mm1

    @loop:
    movdqu xmm0, [eax][ecx]
    movdqu xmm1, [eax][ecx - 1]
    movdqu xmm2, [eax][ecx - 2]
    PAVGB xmm0, xmm1                          // average across RGB
    PAVGB xmm1, xmm2
    PAVGB xmm0, xmm1
    PSUBB xmm0, xmm3                          // signed
    PCMPGTB xmm0, xmm7                        // compare
    PAND xmm0, xmm4                           // greymask
    movdqu [eax][ecx], xmm0                   // store

    sub ecx, MM_SIZE

    movdqu xmm0, [eax][ecx]
    movdqu xmm1, [eax][ecx - 1]
    movdqu xmm2, [eax][ecx - 2]
    PAVGB xmm0, xmm1                          // average across RGB
    PAVGB xmm1, xmm2
    PAVGB xmm0, xmm1
    PSUBB xmm0, xmm3                          // signed
    PCMPGTB xmm0, xmm6                        // compare
    PAND xmm0, xmm4                           // greymask
    movdqu [eax][ecx], xmm0                   // store

    sub ecx, MM_SIZE

    movdqu xmm0, [eax][ecx]
    movdqu xmm1, [eax][ecx - 1]
    movdqu xmm2, [eax][ecx - 2]
    PAVGB xmm0, xmm1                          // average across RGB
    PAVGB xmm1, xmm2
    PAVGB xmm0, xmm1
    PSUBB xmm0, xmm3                          // signed
    PCMPGTB xmm0, xmm5                        // compare
    PAND xmm0, xmm4                           // greymask
    movdqu [eax][ecx], xmm0                   // store

    sub ecx, MM_SIZE

    cmp ecx, 48                               // small strip at end not used to avoid overrun
    jg @loop

    {$else}
    mov eax, pData                            //eax point on first image bit
    sub eax, MM_SIZE                          //16 = sizeof xmm0
    mov ebx, self

    mov ecx, [ebx].TSeuillageProcessor.SampleSize      //1 loop updates 48 bytes
    movq mm3, SignOffset                      //offset pour la conversion non-sign�/sign�
    movq mm4, [ebx].TSeuillageProcessor.GreyMask

    movq mm7, [ebx].TSeuillageProcessor.Mask3          //fixe le seuil3 sur 8 octects ds mm7
    movq mm6, [ebx].TSeuillageProcessor.Mask2          //fixe le seuil2 sur 8 octects ds mm6
    movq mm5, [ebx].TSeuillageProcessor.Mask1          //fixe le seuil1 sur 8 octects ds mm5
  
    @loop:
    movq mm0, [eax][ecx]
    movq mm1, [eax][ecx - 1]
    movq mm2, [eax][ecx - 2]
    PAVGB mm0, mm1                          // average across RGB
    PAVGB mm1, mm2
    PAVGB mm0, mm1
    PSUBB mm0, mm3                          // signed
    PCMPGTB mm0, mm7                        // compare
    PAND mm0, mm4                           // greymask
    movq [eax][ecx], mm0                    // store

    sub ecx, MM_SIZE

    movq mm0, [eax][ecx]
    movq mm1, [eax][ecx - 1]
    movq mm2, [eax][ecx - 2]
    PAVGB mm0, mm1                          // average across RGB
    PAVGB mm1, mm2
    PAVGB mm0, mm1
    PSUBB mm0, mm3                          // signed
    PCMPGTB mm0, mm6                        // compare
    PAND mm0, mm4                           // greymask
    movq [eax][ecx], mm0                    // store

    sub ecx, MM_SIZE

    movq mm0, [eax][ecx]
    movq mm1, [eax][ecx - 1]
    movq mm2, [eax][ecx - 2]
    PAVGB mm0, mm1                          // average across RGB
    PAVGB mm1, mm2
    PAVGB mm0, mm1
    PSUBB mm0, mm3                          // signed
    PCMPGTB mm0, mm5                        // compare
    PAND mm0, mm4                           // greymask
    movq [eax][ecx], mm0                    // store

    sub ecx, MM_SIZE

    cmp ecx, 24                            // small strip at end not used to avoid overrun
    jg @loop
    {$endif}

    emms
  end;
end;

end.

