object frmJvTreeViewLinksEditor: TfrmJvTreeViewLinksEditor
  Left = 294
  Top = 184
  Width = 472
  Height = 267
  ActiveControl = tvItems
  BorderIcons = [biSystemMenu]
  Caption = 'JvTreeViewLinks Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = popTree
  Scaled = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 195
    Width = 464
    Height = 42
    Align = alBottom
    Shape = bsTopLine
  end
  object Splitter1: TSplitter
    Left = 163
    Top = 0
    Width = 3
    Height = 195
    Cursor = crHSplit
    AutoSnap = False
  end
  object Splitter2: TSplitter
    Left = 268
    Top = 0
    Width = 3
    Height = 195
    Cursor = crHSplit
    AutoSnap = False
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 163
    Height = 195
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 4
      Width = 155
      Height = 16
      Align = alTop
      AutoSize = False
      Caption = '&Nodes:'
      FocusControl = tvItems
      Layout = tlCenter
    end
    object tvItems: TTreeView
      Left = 4
      Top = 20
      Width = 155
      Height = 171
      Align = alClient
      ChangeDelay = 60
      HideSelection = False
      Indent = 19
      PopupMenu = popTree
      ReadOnly = True
      ShowButtons = False
      ShowLines = False
      TabOrder = 0
      ToolTips = False
      OnChange = tvItemsChange
    end
  end
  object btnOK: TButton
    Left = 285
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 373
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object pnlMid: TPanel
    Left = 166
    Top = 0
    Width = 102
    Height = 195
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object btnLink: TButton
      Left = 5
      Top = 24
      Width = 91
      Height = 25
      Action = acLink
      Anchors = [akTop]
      TabOrder = 0
      TabStop = False
    end
  end
  object pnlRight: TPanel
    Left = 271
    Top = 0
    Width = 193
    Height = 195
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'pnlRight'
    TabOrder = 2
    object Label2: TLabel
      Left = 4
      Top = 4
      Width = 185
      Height = 17
      Align = alTop
      AutoSize = False
      Caption = '&Pages:'
      FocusControl = lbPages
      Layout = tlCenter
    end
    object lbPages: TListBox
      Left = 4
      Top = 21
      Width = 185
      Height = 170
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object acMain: TActionList
    OnUpdate = acMainUpdate
    Left = 168
    Top = 64
    object acLink: TAction
      Caption = '<- Create &Link ->'
      ShortCut = 16397
      OnExecute = acLinkExecute
    end
  end
  object popTree: TPopupMenu
    Left = 58
    Top = 72
  end
end
