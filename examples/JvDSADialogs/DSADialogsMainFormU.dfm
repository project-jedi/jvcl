object DSADialogsMainForm: TDSADialogsMainForm
  Left = 200
  Top = 189
  Width = 652
  Height = 317
  Caption = 'Don'#39't Show Again (DSA) Examples and tests'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lvDSAInfo: TListView
    Left = 2
    Top = 5
    Width = 628
    Height = 228
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <
      item
        Caption = 'DSA'
        MaxWidth = 40
        MinWidth = 40
        Width = 40
      end
      item
        Alignment = taRightJustify
        Caption = 'DlgID'
        MaxWidth = 50
        MinWidth = 50
      end
      item
        Caption = 'Name'
        MaxWidth = 130
        MinWidth = 130
        Width = 130
      end
      item
        Caption = 'Description'
        MaxWidth = 521
        MinWidth = 400
        Width = 400
      end>
    HideSelection = False
    RowSelect = True
    PopupMenu = pmDSAList
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvDSAInfoChange
    OnDblClick = miExecuteDlgClick
    OnResize = lvDSAInfoResize
  end
  object stbMain: TStatusBar
    Left = 0
    Top = 268
    Width = 644
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Status:'
  end
  object btnClose: TButton
    Left = 556
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object pmDSAList: TPopupMenu
    OnPopup = pmDSAListPopup
    Left = 480
    Top = 70
    object miExecuteDlg: TMenuItem
      Caption = 'Execute...'
      Default = True
      ShortCut = 16397
      OnClick = miExecuteDlgClick
    end
    object miBreak1: TMenuItem
      Caption = '-'
    end
    object miReset: TMenuItem
      Caption = 'Reset'
      ShortCut = 32
      OnClick = miResetClick
    end
  end
end
