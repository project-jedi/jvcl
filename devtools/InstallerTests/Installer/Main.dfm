object FormMain: TFormMain
  Left = 0
  Top = 0
  ActiveControl = clbPersonalities
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Installer'
  ClientHeight = 475
  ClientWidth = 735
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object clbPersonalities: TCheckListBox
    Left = 8
    Top = 8
    Width = 209
    Height = 265
    BevelInner = bvLowered
    BevelKind = bkTile
    BorderStyle = bsNone
    ItemHeight = 18
    Style = lbOwnerDrawFixed
    TabOrder = 0
    OnClick = clbPersonalitiesClick
    OnDrawItem = clbPersonalitiesDrawItem
    OnKeyDown = clbPersonalitiesKeyDown
  end
  object pnlClient: TPanel
    Left = 224
    Top = 8
    Width = 505
    Height = 425
    BevelOuter = bvLowered
    ParentBackground = False
    TabOrder = 1
    object pnlTitle: TPanel
      Left = 1
      Top = 1
      Width = 503
      Height = 48
      Align = alTop
      Color = clWindow
      ParentBackground = False
      TabOrder = 0
      object lblTitle: TLabel
        Left = 8
        Top = 5
        Width = 53
        Height = 18
        Caption = 'lblTitle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblSubTitle: TLabel
        Left = 8
        Top = 27
        Width = 59
        Height = 13
        Caption = 'lblSubTitle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
    object pnlContent: TPanel
      Left = 1
      Top = 49
      Width = 503
      Height = 375
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object pglOptions: TJvPageList
        Left = 3
        Top = 26
        Width = 497
        Height = 346
        ActivePage = spgEmpty
        PropagateEnable = False
        Align = alClient
        ParentBackground = True
        object spgEmpty: TJvStandardPage
          Left = 0
          Top = 0
          Width = 497
          Height = 346
          Caption = 'spgEmpty'
        end
      end
      object tbrOptions: TJvTabBar
        Left = 3
        Top = 3
        Width = 497
        CloseButton = False
        HotTracking = True
        PageList = pglOptions
        Tabs = <>
      end
    end
  end
end
