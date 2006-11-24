object FormStartup: TFormStartup
  Left = 358
  Top = 341
  BorderStyle = bsNone
  ClientHeight = 121
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    Left = 0
    Top = 0
    Width = 416
    Height = 121
    Align = alClient
    ParentBackground = False
    TabOrder = 0
    object Image: TImage
      Left = 1
      Top = 1
      Width = 414
      Height = 17
      Align = alTop
      AutoSize = True
    end
    object ProgressBar: TProgressBar
      Left = 8
      Top = 96
      Width = 401
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
    end
    object PanelInner: TPanel
      Left = 8
      Top = 8
      Width = 401
      Height = 81
      Anchors = [akLeft, akRight, akBottom]
      BevelOuter = bvLowered
      TabOrder = 1
      object LblComponents: TLabel
        Left = 8
        Top = 32
        Width = 96
        Height = 13
        Caption = 'Component count: 0'
      end
      object LblPackages: TLabel
        Left = 8
        Top = 8
        Width = 85
        Height = 13
        Caption = 'Package count: 0'
      end
      object LblComponentName: TLabel
        Left = 300
        Top = 64
        Width = 96
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'LblComponentName'
        Visible = False
      end
      object LblUnits: TLabel
        Left = 8
        Top = 56
        Width = 61
        Height = 13
        Caption = 'Unit count: 0'
      end
      object PaintBox: TPaintBox
        Left = 368
        Top = 8
        Width = 24
        Height = 24
        Anchors = [akTop, akRight]
      end
    end
  end
end
