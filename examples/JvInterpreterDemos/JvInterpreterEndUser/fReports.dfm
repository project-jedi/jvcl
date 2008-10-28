object Reports: TReports
  Left = 156
  Top = 128
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Reports'
  ClientHeight = 255
  ClientWidth = 464
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RAhtLabel1: TJvHTLabel
    Left = 16
    Top = 8
    Width = 433
    Height = 13
    AutoSize = False
    Caption = 'Select report from list and click button <b>Create report</b>'
  end
  object lbReports: TJvHTListBox
    Left = 16
    Top = 32
    Width = 433
    Height = 169
    HideSel = False
    ColorHighlight = clHighlight
    ColorHighlightText = clHighlightText
    ColorDisabledText = clGrayText
    TabOrder = 0
    OnDblClick = bReportClick
  end
  object bReport: TButton
    Left = 248
    Top = 216
    Width = 113
    Height = 25
    Caption = 'Create report'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = bReportClick
  end
  object bCancel: TButton
    Left = 376
    Top = 216
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
  object RegAuto2: TJvFormStorage
    StoredValues = <>
    Left = 88
    Top = 200
  end
  object JvInterpreterFm1: TJvInterpreterFm
    OnGetValue = JvInterpreterFm1GetValue
    Left = 144
    Top = 192
  end
end
