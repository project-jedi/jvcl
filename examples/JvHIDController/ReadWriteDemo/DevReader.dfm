object Form1: TForm1
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
  object Read: TSpeedButton
    Left = 72
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    GroupIndex = 1
    Caption = 'Read'
    OnClick = ReadClick
  end
  object Write: TSpeedButton
    Left = 160
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    Caption = 'Write'
    OnClick = WriteClick
  end
  object Save: TSpeedButton
    Left = 248
    Top = 451
    Width = 81
    Height = 22
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    OnClick = SaveClick
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
  object Info: TSpeedButton
    Left = 8
    Top = 451
    Width = 49
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Info'
    OnClick = InfoClick
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
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files *.txt|*.txt|All Files *.*|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing, ofDontAddToRecent]
    Title = 'Save History'
    Left = 312
  end
  object HidCtl: TJvHidDeviceController
    OnEnumerate = HidCtlEnumerate
    OnDeviceChange = HidCtlDeviceChange
    Left = 368
  end
end
