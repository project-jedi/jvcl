object frmGrpOverviewGen: TfrmGrpOverviewGen
  Left = 300
  Top = 163
  BorderStyle = bsSingle
  Caption = 'Group overview generator'
  ClientHeight = 540
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnBuild: TButton
    Left = 10
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Build'
    TabOrder = 0
    OnClick = btnBuildClick
  end
  object mmLog: TMemo
    Left = 5
    Top = 35
    Width = 782
    Height = 500
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object cxIncludeIcons: TCheckBox
    Left = 95
    Top = 10
    Width = 126
    Height = 17
    Caption = 'Include palette icons'
    TabOrder = 1
  end
end
