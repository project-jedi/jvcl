object frmDSAExamplesCustomDlg1: TfrmDSAExamplesCustomDlg1
  Left = 443
  Top = 325
  BorderIcons = [biSystemMenu, biHelp]
  Caption = 'Custom dialog 1'
  ClientHeight = 121
  ClientWidth = 165
  Color = clBtnFace
  Constraints.MaxHeight = 157
  Constraints.MinHeight = 157
  Constraints.MinWidth = 178
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  DesignSize = (
    165
    121)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 10
    Top = 10
    Width = 153
    Height = 56
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This is a simple dialog with no real function at all. Just a sim' +
      'ple test of TJvDSADialog functionality.'
    WordWrap = True
    ExplicitWidth = 150
  end
  object cxSuppress: TCheckBox
    Left = 10
    Top = 75
    Width = 153
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'cxSuppress'
    TabOrder = 0
    ExplicitWidth = 150
  end
  object btnOK: TButton
    Left = 45
    Top = 95
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object JvDSADialog: TJvDSADialog
    Timeout = 0
    CheckControl = cxSuppress
    DialogID = 7
    IgnoreDSAChkMrkTxt = False
    Left = 105
    Top = 30
  end
end
