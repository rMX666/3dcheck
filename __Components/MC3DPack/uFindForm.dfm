object FSearchForm: TFSearchForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1055#1086#1080#1089#1082
  ClientHeight = 235
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageSearchReplace: TPageControl
    Left = 0
    Top = 0
    Width = 374
    Height = 235
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    OnChange = PageSearchReplaceChange
    object TabSheet1: TTabSheet
      Caption = #1055#1086#1080#1089#1082
      DesignSize = (
        366
        207)
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 61
        Height = 13
        Alignment = taRightJustify
        Caption = #1063#1090#1086' '#1080#1089#1082#1072#1090#1100':'
      end
      object EditSearch: TEdit
        Left = 72
        Top = 8
        Width = 289
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object gbSearchOptions: TGroupBox
        Left = 8
        Top = 57
        Width = 225
        Height = 113
        Caption = #1054#1087#1094#1080#1080' '#1087#1086#1080#1089#1082#1072
        TabOrder = 1
        object cbSearchCaseSensitive: TCheckBox
          Left = 8
          Top = 17
          Width = 177
          Height = 17
          Caption = #1063#1091#1074#1089#1090#1074#1080#1090#1077#1083#1100#1085#1086#1089#1090#1100' '#1082' '#1088#1077#1075#1080#1089#1090#1088#1091
          TabOrder = 0
        end
        object cbSearchWholeWords: TCheckBox
          Left = 8
          Top = 39
          Width = 121
          Height = 17
          Caption = #1058#1086#1083#1100#1082#1086' '#1094#1077#1083#1099#1077' '#1089#1083#1086#1074#1072
          TabOrder = 1
        end
        object cbSearchFromCursor: TCheckBox
          Left = 8
          Top = 61
          Width = 145
          Height = 17
          Caption = #1055#1086#1080#1089#1082' '#1089' '#1087#1086#1079#1080#1094#1080#1080' '#1082#1091#1088#1089#1086#1088#1072
          TabOrder = 2
        end
        object cbSearchSelectedOnly: TCheckBox
          Left = 8
          Top = 83
          Width = 209
          Height = 17
          Caption = #1055#1086#1080#1089#1082' '#1090#1086#1083#1100#1082#1086' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1090#1077#1082#1089#1090#1091
          TabOrder = 3
        end
      end
      object rgSearchDirection: TRadioGroup
        Left = 243
        Top = 56
        Width = 119
        Height = 65
        Caption = #1053#1072#1087#1088#1072#1074#1083#1077#1085#1080#1077
        ItemIndex = 0
        Items.Strings = (
          #1042#1087#1077#1088#1077#1076
          #1053#1072#1079#1072#1076)
        TabOrder = 2
      end
      object btnOk: TButton
        Left = 200
        Top = 177
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = #1053#1072#1081#1090#1080
        TabOrder = 3
        OnClick = btnOkClick
      end
      object btnCancel: TButton
        Left = 286
        Top = 177
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = #1047#1072#1082#1088#1099#1090#1100
        TabOrder = 4
        OnClick = btnCancelClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1047#1072#1084#1077#1085#1072
      ImageIndex = 1
      DesignSize = (
        366
        207)
      object Label3: TLabel
        Left = 2
        Top = 32
        Width = 67
        Height = 13
        Alignment = taRightJustify
        Caption = #1047#1072#1084#1077#1085#1080#1090#1100' '#1085#1072':'
      end
      object EditReplace: TEdit
        Left = 72
        Top = 32
        Width = 289
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
  end
end
