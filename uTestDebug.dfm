object fTestDebug: TfTestDebug
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = #1054#1082#1085#1086' '#1086#1090#1083#1072#1076#1082#1080' '#1080#1089#1087#1099#1090#1072#1085#1080#1103
  ClientHeight = 540
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  ScreenSnap = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mDebug: TMemo
    Left = 0
    Top = 0
    Width = 684
    Height = 515
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 176
    ExplicitTop = 184
    ExplicitWidth = 185
    ExplicitHeight = 89
  end
  object btnClose: TButton
    Left = 0
    Top = 515
    Width = 684
    Height = 25
    Align = alBottom
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 1
    OnClick = btnCloseClick
    ExplicitLeft = 72
    ExplicitTop = 344
    ExplicitWidth = 75
  end
end
