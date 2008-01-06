object JvPropertyStoreEditorForm: TJvPropertyStoreEditorForm
  Left = 0
  Top = 0
  ClientHeight = 559
  ClientWidth = 833
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 525
    ExplicitLeft = 225
    ExplicitTop = 100
    ExplicitHeight = 100
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 525
    Width = 833
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object BottomButtonPanel: TPanel
      Left = 667
      Top = 0
      Width = 166
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkButton: TButton
        Left = 4
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Ok'
        ModalResult = 1
        TabOrder = 0
        OnClick = OkButtonClick
      end
      object CancelButton: TButton
        Left = 85
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        OnClick = CancelButtonClick
      end
    end
  end
  object EditPanel: TPanel
    Left = 188
    Top = 0
    Width = 645
    Height = 525
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object ListPanel: TPanel
      Left = 0
      Top = 15
      Width = 241
      Height = 416
      BevelOuter = bvNone
      TabOrder = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 141
        Width = 241
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 132
        ExplicitWidth = 185
      end
      object ListButtonPanel: TPanel
        Left = 0
        Top = 144
        Width = 241
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 185
        object ListInsertButton: TButton
          Left = 0
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Insert'
          TabOrder = 0
          OnClick = ListInsertButtonClick
        end
        object ListEditButton: TButton
          Left = 80
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Edit'
          TabOrder = 1
          OnClick = ListEditButtonClick
        end
        object ListDeleteButton: TButton
          Left = 120
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Delete'
          TabOrder = 2
          OnClick = ListDeleteButtonClick
        end
        object ListCopyButton: TButton
          Left = 40
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Copy'
          TabOrder = 3
          OnClick = ListCopyButtonClick
        end
        object ListUpButton: TButton
          Left = 166
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Up'
          TabOrder = 4
          OnClick = ListUpButtonClick
        end
        object ListDownButton: TButton
          Left = 205
          Top = 0
          Width = 40
          Height = 22
          Caption = '&Down'
          TabOrder = 5
          OnClick = ListDownButtonClick
        end
      end
      object ListInspectorPanel: TPanel
        Left = 0
        Top = 0
        Width = 241
        Height = 141
        Align = alTop
        BevelOuter = bvNone
        Caption = 'ListInspectorPanel'
        TabOrder = 1
        ExplicitWidth = 185
      end
      object ListBox: TJvListBox
        Left = 0
        Top = 169
        Width = 241
        Height = 247
        Align = alClient
        ItemHeight = 13
        Background.FillMode = bfmTile
        Background.Visible = False
        ParentFlat = False
        TabOrder = 2
      end
    end
    object InspectorPanel: TPanel
      Left = 300
      Top = 15
      Width = 284
      Height = 386
      BevelOuter = bvNone
      TabOrder = 1
      object JvInspector: TJvInspector
        Left = 0
        Top = 0
        Width = 284
        Height = 386
        Align = alClient
        AutoDropDown = True
        Divider = 150
        ItemHeight = 16
        WantTabs = True
        AfterItemCreate = JvInspectorAfterItemCreate
        BeforeItemCreate = JvInspectorBeforeItemCreate
        ExplicitLeft = 60
        ExplicitTop = 35
      end
    end
  end
  object TreePanel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 525
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object PropertyStoreTreeView: TJvTreeView
      Left = 3
      Top = 3
      Width = 179
      Height = 519
      Align = alClient
      HideSelection = False
      HotTrack = True
      Indent = 19
      RowSelect = True
      TabOrder = 0
      OnChange = PropertyStoreTreeViewChange
      LineColor = clScrollBar
    end
  end
end
