object frmDSAExamplesProgressDlg: TfrmDSAExamplesProgressDlg
  Left = 415
  Top = 266
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Queue test form.'
  ClientHeight = 162
  ClientWidth = 245
  Color = clBtnFace
  Constraints.MaxHeight = 187
  Constraints.MinHeight = 187
  Constraints.MinWidth = 253
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object lblItemCount: TLabel
    Left = 10
    Top = 10
    Width = 123
    Height = 13
    Caption = 'Enter the number of items:'
  end
  object lblNote: TLabel
    Left = 20
    Top = 30
    Width = 205
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Note: You can only end this test by selecting the Cancel or No o' +
      'ption from the message dialog. When the dialog is suppressed, yo' +
      'u can not end the progress.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object edItems: TEdit
    Left = 140
    Top = 5
    Width = 100
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = '1000'
  end
  object btnStart: TButton
    Left = 45
    Top = 105
    Width = 75
    Height = 25
    Caption = 'Start'
    Default = True
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnCancel: TButton
    Left = 125
    Top = 105
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pbProgress: TJvProgressBar
    Left = 20
    Top = 135
    Width = 210
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Smooth = True
    TabOrder = 3
  end
end
