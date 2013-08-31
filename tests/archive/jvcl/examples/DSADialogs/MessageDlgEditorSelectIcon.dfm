object frmMessageDlgEditorSelectIcon: TfrmMessageDlgEditorSelectIcon
  Left = 432
  Top = 337
  BorderStyle = bsDialog
  Caption = 'Select picture...'
  ClientHeight = 240
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object rbWarning: TRadioButton
    Left = 5
    Top = 35
    Width = 130
    Height = 13
    Caption = 'Use mtWarning'
    TabOrder = 0
  end
  object rbError: TRadioButton
    Left = 5
    Top = 60
    Width = 130
    Height = 13
    Caption = 'Use mtError'
    TabOrder = 1
  end
  object rbInformation: TRadioButton
    Left = 5
    Top = 85
    Width = 130
    Height = 13
    Caption = 'Use mtInformation'
    TabOrder = 2
  end
  object rbConfirmation: TRadioButton
    Left = 5
    Top = 110
    Width = 130
    Height = 13
    Caption = 'Use mtConfirmation'
    TabOrder = 3
  end
  object rbNone: TRadioButton
    Left = 5
    Top = 10
    Width = 130
    Height = 13
    Caption = 'None'
    TabOrder = 4
  end
  object rbIconRes: TRadioButton
    Left = 5
    Top = 135
    Width = 130
    Height = 13
    Caption = 'Load icon resource'
    TabOrder = 5
  end
  object rbBMPRes: TRadioButton
    Left = 5
    Top = 160
    Width = 130
    Height = 13
    Caption = 'Load bitmap resource'
    TabOrder = 6
  end
  object rbLoadFile: TRadioButton
    Left = 5
    Top = 185
    Width = 130
    Height = 13
    Caption = 'Load from file'
    TabOrder = 7
  end
  object edIconRes: TEdit
    Left = 140
    Top = 130
    Width = 195
    Height = 21
    TabOrder = 8
    OnChange = ResNameChanged
  end
  object edBMPRes: TEdit
    Left = 140
    Top = 155
    Width = 195
    Height = 21
    TabOrder = 9
    OnChange = ResNameChanged
  end
  object edFileName: TJvFilenameEdit
    Left = 140
    Top = 180
    Width = 195
    Height = 21
    DialogKind = dkOpenPicture
    DialogOptions = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    DialogTitle = 'Select image to load...'
    ButtonFlat = True
    NumGlyphs = 1
    TabOrder = 10
    OnChange = edFileNameChange
  end
  object btnOK: TButton
    Left = 180
    Top = 210
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 11
  end
  object btnCancel: TButton
    Left = 260
    Top = 210
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 12
  end
end
