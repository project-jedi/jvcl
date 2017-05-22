object MainForm: TMainForm
  Left = 192
  Top = 116
  AutoScroll = False
  Caption = 'HID Reader'
  ClientHeight = 484
  ClientWidth = 444
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnDestroy = DevListBoxClick
  PixelsPerInch = 96
  TextHeight = 16
  object ReadBtn: TSpeedButton
    Left = 72
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    GroupIndex = 1
    Caption = 'Read'
    OnClick = ReadBtnClick
  end
  object WriteBtn: TSpeedButton
    Left = 160
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    Caption = 'Write'
    OnClick = WriteBtnClick
  end
  object SaveBtn: TSpeedButton
    Left = 248
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    OnClick = SaveBtnClick
  end
  object Label1: TLabel
    Left = 41
    Top = 300
    Width = 64
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'ReportID'
  end
  object InfoBtn: TSpeedButton
    Left = 8
    Top = 451
    Width = 57
    Height = 22
    Caption = 'Info'
    OnClick = InfoBtnClick
  end
  object DevListBox: TListBox
    Left = -1
    Top = 0
    Width = 444
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 0
    OnClick = DevListBoxClick
  end
  object HistoryListBox: TListBox
    Left = 0
    Top = 84
    Width = 444
    Height = 205
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 1
  end
  object ReportID: TEdit
    Left = 8
    Top = 296
    Width = 25
    Height = 24
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 8
    Top = 328
    Width = 25
    Height = 24
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files *.txt|*.txt|All Files *.*|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing, ofDontAddToRecent]
    Title = 'Save History'
    Left = 312
  end
  object HidCtl: TJvHidDeviceController
    OnEnumerate = HidCtlEnumerate
    OnDeviceChange = HidCtlDeviceChange
    OnDeviceData = ShowRead
    OnDeviceDataError = HidCtlDeviceDataError
    Left = 368
  end
end
