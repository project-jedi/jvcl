object frmGenDonate: TfrmGenDonate
  Left = 312
  Top = 204
  AutoScroll = False
  Caption = 'JVCL Donator List generator'
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
  object lblInfo: TLabel
    Left = 165
    Top = 510
    Width = 622
    Height = 13
    Anchors = [akTop, akRight, akBottom]
    AutoSize = False
  end
  object btnScan: TButton
    Left = 5
    Top = 510
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Scan'
    TabOrder = 0
    OnClick = btnScanClick
  end
  object btnWrite: TButton
    Left = 85
    Top = 510
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Write'
    TabOrder = 1
    OnClick = btnWriteClick
  end
  object lvAuthors: TListView
    Left = 5
    Top = 5
    Width = 782
    Height = 495
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Full Name'
        Width = 389
      end
      item
        Caption = 'Sorting name'
        Width = 389
      end>
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = pmChangeAuthor
    TabOrder = 2
    ViewStyle = vsReport
    OnChanging = lvAuthorsChanging
    OnData = lvAuthorsData
  end
  object edAuthor: TEdit
    Left = 395
    Top = 25
    Width = 391
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    Visible = False
    OnExit = edAuthorExit
    OnKeyPress = edAuthorKeyPress
  end
  object pmChangeAuthor: TPopupMenu
    OnPopup = pmChangeAuthorPopup
    Left = 415
    Top = 110
    object miEditSortName: TMenuItem
      Caption = 'Edit'
      Default = True
      ShortCut = 16397
      OnClick = miEditSortNameClick
    end
    object miIgnoreAuthor: TMenuItem
      Caption = 'Ignore'
      ShortCut = 16457
      OnClick = miIgnoreAuthorClick
    end
  end
  object AppEvents: TApplicationEvents
    OnIdle = AppEventsIdle
    Left = 340
    Top = 160
  end
end
