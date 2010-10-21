object FConfirmDialog: TFConfirmDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1055#1086#1076#1090#1074#1077#1088#1078#1076#1077#1085#1080#1077' '#1079#1072#1084#1077#1085#1099
  ClientHeight = 97
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    329
    97)
  PixelsPerInch = 96
  TextHeight = 13
  object lblConfirmation: TLabel
    Left = 60
    Top = 12
    Width = 261
    Height = 44
    AutoSize = False
    WordWrap = True
  end
  object Image1: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object btnReplace: TButton
    Left = 8
    Top = 67
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = #1044#1072
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 87
    Top = 67
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = #1053#1077#1090
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 167
    Top = 67
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 166
  end
  object btnReplaceAll: TButton
    Left = 246
    Top = 67
    Width = 75
    Height = 23
    Anchors = [akTop, akRight]
    Caption = #1044#1072' '#1076#1083#1103' '#1074#1089#1077#1093
    ModalResult = 10
    TabOrder = 3
    ExplicitLeft = 245
  end
end
