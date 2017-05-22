object frmChangeNotificationDirDlg: TfrmChangeNotificationDirDlg
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Add directory to watch'
  ClientHeight = 245
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Directory:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 72
    Width = 353
    Height = 105
    Caption = ' Watch for: '
    TabOrder = 0
    object cbAttributes: TCheckBox
      Left = 16
      Top = 72
      Width = 126
      Height = 17
      Caption = '&Attributes change'
      TabOrder = 0
    end
    object cbDirNames: TCheckBox
      Left = 16
      Top = 48
      Width = 126
      Height = 17
      Caption = '&Directory change'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbFileNames: TCheckBox
      Left = 16
      Top = 24
      Width = 126
      Height = 17
      Caption = '&File change'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbSize: TCheckBox
      Left = 182
      Top = 24
      Width = 126
      Height = 17
      Caption = '&Size change'
      TabOrder = 3
    end
    object cbWrite: TCheckBox
      Left = 182
      Top = 48
      Width = 126
      Height = 17
      Caption = '&Write time change'
      TabOrder = 4
    end
    object cbSubTrees: TCheckBox
      Left = 182
      Top = 72
      Width = 142
      Height = 17
      Caption = '&Include subtrees'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
  end
  object Edit1: TEdit
    Left = 16
    Top = 32
    Width = 233
    Height = 21
    TabOrder = 1
  end
  object Button3: TButton
    Left = 288
    Top = 208
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Button1: TButton
    Left = 264
    Top = 32
    Width = 75
    Height = 25
    Caption = '&Browse...'
    TabOrder = 3
    OnClick = Button1Click
  end
  object btnOK: TButton
    Left = 200
    Top = 208
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
end
