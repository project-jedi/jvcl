object frmModules: TfrmModules
  Left = 427
  Top = 161
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'Additional Modules for Error Searching '
  ClientHeight = 228
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  Scaled = false
  OnHelp = FormHelp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 69
    Height = 13
    Caption = '&Module Name:'
    FocusControl = edModuleName
  end
  object edModuleName: TEdit
    Left = 96
    Top = 16
    Width = 201
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 304
    Top = 16
    Width = 21
    Height = 21
    Action = acBrowse
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 179
    Top = 40
    Width = 55
    Height = 21
    Action = acAdd
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object btnRemove: TButton
    Left = 240
    Top = 40
    Width = 55
    Height = 21
    Action = acRemove
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 256
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object btnOK: TButton
    Left = 176
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object lbModules: TListBox
    Left = 8
    Top = 72
    Width = 321
    Height = 110
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
  end
  object alModules: TActionList
    OnUpdate = alModulesUpdate
    Left = 136
    Top = 96
    object acBrowse: TAction
      Caption = '...'
      ShortCut = 16397
      OnExecute = acBrowseExecute
    end
    object acAdd: TAction
      Caption = '&Add'
      ShortCut = 16429
      OnExecute = acAddExecute
    end
    object acRemove: TAction
      Caption = '&Remove'
      ShortCut = 16430
      OnExecute = acRemoveExecute
    end
    object acHelp: TAction
      Caption = '&Help'
      ShortCut = 112
      Visible = False
      OnExecute = acHelpExecute
    end
  end
end
