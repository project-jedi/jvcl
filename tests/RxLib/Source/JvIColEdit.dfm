object IconListDialog: TIconListDialog
  Left = 236
  Top = 149
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Icon List Editor'
  ClientHeight = 147
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = UpdateClipboard
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = UpdateClipboard
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 37
    Width = 253
    Height = 20
  end
  object Label1: TLabel
    Left = 9
    Top = 40
    Width = 66
    Height = 13
    Caption = 'Icons Count:  '
  end
  object CntLabel: TLabel
    Left = 78
    Top = 40
    Width = 39
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '12'
  end
  object Label3: TLabel
    Left = 125
    Top = 40
    Width = 79
    Height = 13
    Caption = 'Selected index:  '
  end
  object IdxLabel: TLabel
    Left = 208
    Top = 40
    Width = 41
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '3'
  end
  object OK: TButton
    Left = 268
    Top = 37
    Width = 77
    Height = 24
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Cancel: TButton
    Left = 268
    Top = 65
    Width = 77
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Holder: TPanel
    Left = 4
    Top = 62
    Width = 253
    Height = 59
    BevelInner = bvLowered
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object Slot0: TPanel
      Left = 10
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      Color = clActiveCaption
      TabOrder = 0
      OnMouseDown = ImageMouseDown
      object Image0: TImage
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot1: TPanel
      Tag = 1
      Left = 58
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 1
      OnMouseDown = ImageMouseDown
      object Image1: TImage
        Tag = 1
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot2: TPanel
      Tag = 2
      Left = 106
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 2
      OnMouseDown = ImageMouseDown
      object Image2: TImage
        Tag = 2
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot3: TPanel
      Tag = 3
      Left = 154
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 3
      OnMouseDown = ImageMouseDown
      object Image3: TImage
        Tag = 3
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
    object Slot4: TPanel
      Tag = 4
      Left = 202
      Top = 10
      Width = 40
      Height = 40
      BevelInner = bvLowered
      BevelOuter = bvNone
      TabOrder = 4
      OnMouseDown = ImageMouseDown
      object Image4: TImage
        Tag = 4
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        OnMouseDown = ImageMouseDown
      end
    end
  end
  object ScrollBar: TScrollBar
    Left = 4
    Top = 124
    Width = 253
    Height = 17
    LargeChange = 5
    PageSize = 0
    TabOrder = 3
    OnChange = ScrollBarChange
  end
end
