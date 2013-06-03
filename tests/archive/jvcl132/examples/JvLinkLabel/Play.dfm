object frmPlay: TfrmPlay
  Left = 448
  Top = 517
  Width = 541
  Height = 419
  BorderStyle = bsSizeToolWin
  Caption = 'Play with the TJvLinkLabel!'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object LinkLabel: TJvLinkLabel
    Left = 0
    Top = 0
    Width = 345
    Height = 351
    Transparent = False
    LinkColor = clBlue
    LinkColorClicked = clRed
    LinkColorHot = clPurple
    LinkStyle = [fsUnderline]
    HotLinks = False
    AutoHeight = True
    MarginWidth = 4
    MarginHeight = 4
    Align = alClient
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Splitter1: TSplitter
    Left = 345
    Top = 0
    Width = 3
    Height = 351
    Cursor = crHSplit
    Align = alRight
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 348
    Top = 0
    Width = 185
    Height = 351
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btnRefresh: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshClick
      end
    end
    object TreeView: TTreeView
      Left = 0
      Top = 41
      Width = 185
      Height = 310
      Align = alClient
      Indent = 19
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 351
    Width = 533
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 533
      Height = 2
      Align = alTop
    end
    object Edit: TEdit
      Left = 8
      Top = 8
      Width = 518
      Height = 22
      TabOrder = 0
      OnChange = EditChange
    end
  end
end
