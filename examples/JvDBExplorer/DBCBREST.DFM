object RestructureDialog: TRestructureDialog
  Left = 276
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Restructure Warning'
  ClientHeight = 187
  ClientWidth = 378
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
  object ObjTypeLabel: TJvLabel
    Left = 12
    Top = 96
    Width = 69
    Height = 13
    Caption = 'Object Type:  '
    FocusControl = ObjType
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
  end
  object ObjDescLabel: TJvLabel
    Left = 12
    Top = 116
    Width = 98
    Height = 13
    Caption = 'Object Description:  '
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
  end
  object OkBtn: TButton
    Left = 294
    Top = 15
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 294
    Top = 47
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object MessageGrp: TGroupBox
    Left = 7
    Top = 3
    Width = 278
    Height = 82
    TabOrder = 2
    object IconImage: TImage
      Left = 9
      Top = 15
      Width = 32
      Height = 32
      AutoSize = True
    end
    object YesBtn: TRadioButton
      Tag = 308
      Left = 220
      Top = 16
      Width = 45
      Height = 20
      Caption = '&Yes'
      TabOrder = 0
      TabStop = True
      Visible = False
    end
    object NoBtn: TRadioButton
      Tag = 309
      Left = 220
      Top = 36
      Width = 45
      Height = 20
      Caption = '&No'
      TabOrder = 1
      Visible = False
    end
    object MsgLabel: TMemo
      Left = 49
      Top = 12
      Width = 166
      Height = 63
      TabStop = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentColor = True
      ParentCtl3D = False
      ReadOnly = True
      TabOrder = 2
    end
  end
  object ObjType: TEdit
    Left = 92
    Top = 92
    Width = 193
    Height = 21
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object ObjDesc: TMemo
    Left = 12
    Top = 133
    Width = 273
    Height = 49
    TabStop = False
    ParentColor = True
    ReadOnly = True
    TabOrder = 4
  end
end
