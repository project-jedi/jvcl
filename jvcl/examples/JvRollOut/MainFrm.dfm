object Form1: TForm1
  Left = 265
  Top = 125
  Width = 816
  Height = 562
  Caption = 'JvRollOut Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 280
    Top = 0
    Width = 5
    Height = 428
    Cursor = crSizeWE
    AutoSnap = False
    Color = clMaroon
    MinSize = 200
    ParentColor = False
  end
  object Panel4: TPanel
    Left = 0
    Top = 428
    Width = 808
    Height = 88
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object chkShowFocus: TCheckBox
      Left = 16
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Show Focus'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkShowFocusClick
    end
    object chkTabStop: TCheckBox
      Left = 16
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Tab Stop'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkTabStopClick
    end
    object chkToggleAnywhere: TCheckBox
      Left = 128
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Toggle Anywhere'
      TabOrder = 2
      OnClick = chkToggleAnywhereClick
    end
    object chkGroupIndex: TCheckBox
      Left = 128
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Group'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chkGroupIndexClick
    end
    object chkHideButton: TCheckBox
      Left = 264
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Hide Button Frame'
      TabOrder = 4
      OnClick = chkHideButtonClick
    end
    object chkHideFrame: TCheckBox
      Left = 264
      Top = 40
      Width = 121
      Height = 17
      Caption = 'Hide Rollout Frame'
      TabOrder = 5
      OnClick = chkHideFrameClick
    end
    object chkImages: TCheckBox
      Left = 400
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Images'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = chkImagesClick
    end
  end
  object Panel3: TPanel
    Left = 285
    Top = 0
    Width = 523
    Height = 428
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 0
      Top = 265
      Width = 523
      Height = 5
      Cursor = crSizeNS
      Align = alTop
      AutoSnap = False
      Color = clMaroon
      MinSize = 200
      ParentColor = False
    end
    object pnlRightAlign: TPanel
      Left = 0
      Top = 270
      Width = 523
      Height = 158
      Align = alClient
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 0
      object Label3: TLabel
        Left = 433
        Top = 0
        Width = 90
        Height = 158
        Align = alRight
        Alignment = taCenter
        AutoSize = False
        Caption = 'Right Aligned'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object RO40: TJvRollOut
        Left = 288
        Top = 0
        Width = 145
        Height = 158
        Align = alRight
        Caption = 'T'
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 9
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 152
        FCWidth = 22
        FCHeight = 152
      end
      object RO39: TJvRollOut
        Left = 266
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'S'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 8
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO38: TJvRollOut
        Left = 244
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'R'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 7
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO37: TJvRollOut
        Left = 222
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'Q'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 6
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO36: TJvRollOut
        Left = 200
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'P && &Z'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 5
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO35: TJvRollOut
        Left = 178
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'O && &Y'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 4
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO34: TJvRollOut
        Left = 156
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'N && &X'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 3
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO33: TJvRollOut
        Left = 134
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'M && &W'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 2
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO32: TJvRollOut
        Left = 112
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'L && &V'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 1
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 152
      end
      object RO31: TJvRollOut
        Left = 90
        Top = 0
        Width = 22
        Height = 158
        Align = alRight
        Caption = 'K && &U'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 3
        ImageOptions.IndexExpanded = 2
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 0
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 152
        FCWidth = 22
        FCHeight = 152
      end
    end
    object pnlLeftAlign: TPanel
      Left = 0
      Top = 0
      Width = 523
      Height = 265
      Align = alTop
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 1
      object Label4: TLabel
        Left = 0
        Top = 0
        Width = 90
        Height = 265
        Align = alLeft
        Alignment = taCenter
        AutoSize = False
        Caption = 'Left Aligned'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object RO30: TJvRollOut
        Left = 534
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'J && &T'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 9
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 129
        FCWidth = 22
        FCHeight = 22
      end
      object RO29: TJvRollOut
        Left = 389
        Top = 0
        Width = 145
        Height = 265
        Align = alLeft
        Caption = 'I && &S'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 8
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 145
        FCHeight = 129
      end
      object RO28: TJvRollOut
        Left = 367
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'H && &R'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 7
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO27: TJvRollOut
        Left = 345
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'G && &Q'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 6
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO26: TJvRollOut
        Left = 323
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'F && &P'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 5
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO25: TJvRollOut
        Left = 301
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'E && &O'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 4
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO24: TJvRollOut
        Left = 279
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'D && &N'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 3
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO23: TJvRollOut
        Left = 257
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'C && &M'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 2
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO22: TJvRollOut
        Left = 235
        Top = 0
        Width = 22
        Height = 265
        Align = alLeft
        Caption = 'B && &L'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 1
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
      object RO21: TJvRollOut
        Left = 90
        Top = 0
        Width = 145
        Height = 265
        Align = alLeft
        Caption = 'A && &K'
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 7
        ImageOptions.IndexExpanded = 6
        ImageOptions.Images = ImageList1
        Placement = plLeft
        TabOrder = 0
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 129
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 280
    Height = 428
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 144
      Top = 0
      Width = 5
      Height = 428
      Cursor = crSizeWE
      AutoSnap = False
      Color = clMaroon
      MinSize = 100
      ParentColor = False
    end
    object pnlTopAlign: TPanel
      Left = 0
      Top = 0
      Width = 144
      Height = 428
      Align = alLeft
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 0
      object Label1: TLabel
        Left = 0
        Top = 0
        Width = 144
        Height = 40
        Align = alTop
        Alignment = taCenter
        AutoSize = False
        Caption = 'Top Aligned'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object RO1: TJvRollOut
        Left = 0
        Top = 40
        Width = 144
        Height = 22
        Action = JvRollOutAction1
        Align = alTop
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 0
        TabStop = True
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
        object Edit1: TEdit
          Left = 8
          Top = 40
          Width = 121
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Edit2: TEdit
          Left = 8
          Top = 72
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'Edit2'
        end
        object Edit3: TEdit
          Left = 8
          Top = 104
          Width = 121
          Height = 21
          TabOrder = 2
          Text = 'Edit3'
        end
        object Edit4: TEdit
          Left = 8
          Top = 136
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'Edit4'
        end
      end
      object RO2: TJvRollOut
        Left = 0
        Top = 62
        Width = 144
        Height = 22
        Action = JvRollOutAction2
        Align = alTop
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 1
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
        object Edit5: TEdit
          Left = 8
          Top = 40
          Width = 121
          Height = 21
          TabStop = False
          Color = clBtnFace
          TabOrder = 0
          Text = 'Edit1'
        end
        object Edit6: TEdit
          Left = 8
          Top = 72
          Width = 121
          Height = 21
          TabStop = False
          Color = clBtnFace
          TabOrder = 1
          Text = 'Edit2'
        end
        object Edit7: TEdit
          Left = 8
          Top = 104
          Width = 121
          Height = 21
          TabOrder = 2
          Text = 'Edit3'
        end
        object Edit8: TEdit
          Left = 8
          Top = 136
          Width = 121
          Height = 21
          TabOrder = 3
          Text = 'Edit4'
        end
      end
      object RO3: TJvRollOut
        Left = 0
        Top = 84
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&3'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 2
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
        object Edit9: TEdit
          Left = 8
          Top = 40
          Width = 121
          Height = 21
          TabOrder = 0
          Text = 'Edit1'
        end
        object Edit10: TEdit
          Left = 8
          Top = 72
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'Edit2'
        end
        object Edit11: TEdit
          Left = 8
          Top = 104
          Width = 121
          Height = 21
          TabStop = False
          Color = clBtnFace
          TabOrder = 2
          Text = 'Edit3'
        end
        object Edit12: TEdit
          Left = 8
          Top = 136
          Width = 121
          Height = 21
          TabStop = False
          Color = clBtnFace
          TabOrder = 3
          Text = 'Edit4'
        end
      end
      object RO4: TJvRollOut
        Left = 0
        Top = 106
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&4'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 3
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO5: TJvRollOut
        Left = 0
        Top = 128
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&5'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 4
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO6: TJvRollOut
        Left = 0
        Top = 150
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&6'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 5
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO7: TJvRollOut
        Left = 0
        Top = 172
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&7'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 6
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO8: TJvRollOut
        Left = 0
        Top = 194
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&8'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 7
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO9: TJvRollOut
        Left = 0
        Top = 216
        Width = 144
        Height = 22
        Align = alTop
        Caption = '&9'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 8
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO10: TJvRollOut
        Left = 0
        Top = 238
        Width = 144
        Height = 22
        Align = alTop
        Caption = '1&0'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.Images = ImageList1
        TabOrder = 9
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
    end
    object pnlBottomAlign: TPanel
      Left = 149
      Top = 0
      Width = 131
      Height = 428
      Align = alClient
      BevelOuter = bvNone
      Color = clGray
      TabOrder = 1
      object Label2: TLabel
        Left = 0
        Top = 388
        Width = 131
        Height = 40
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 'Bottom Aligned'
        Color = clGray
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -13
        Font.Name = 'MS Shell Dlg 2'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object RO20: TJvRollOut
        Left = 0
        Top = 218
        Width = 131
        Height = 170
        Align = alBottom
        Caption = '&J'
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 9
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO19: TJvRollOut
        Left = 0
        Top = 196
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&I'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 8
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO18: TJvRollOut
        Left = 0
        Top = 174
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&H'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 7
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO17: TJvRollOut
        Left = 0
        Top = 152
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&G'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 6
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO16: TJvRollOut
        Left = 0
        Top = 130
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&F'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 5
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO15: TJvRollOut
        Left = 0
        Top = 108
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&E'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 4
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO14: TJvRollOut
        Left = 0
        Top = 86
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&D'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 3
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO13: TJvRollOut
        Left = 0
        Top = 64
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&C'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 2
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO12: TJvRollOut
        Left = 0
        Top = 42
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&B'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 1
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
      object RO11: TJvRollOut
        Left = 0
        Top = 20
        Width = 131
        Height = 22
        Align = alBottom
        Caption = '&A'
        Collapsed = True
        GroupIndex = 1
        ImageOptions.IndexCollapsed = 5
        ImageOptions.IndexExpanded = 4
        ImageOptions.Images = ImageList1
        TabOrder = 0
        TabStop = True
        ToggleAnywhere = False
        FAWidth = 145
        FAHeight = 170
        FCWidth = 22
        FCHeight = 22
      end
    end
  end
  object ImageList1: TImageList
    Left = 157
    Top = 10
    Bitmap = {
      494C010109000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001001000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010001000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000100000000000
      1000000000001000100000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000100000000000
      1000000010000000000010000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000100000000000
      1000000010000000000010000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010001000
      1000000010000000000010000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1000000010001000100000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1000000010000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010421042104210421042
      1042104210421042104210421042104200000000104210421042104210421042
      1042104210421042104210421042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042FF7F1863E07F1863
      E07F1863E07F1863E07FE07F10420000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F00001042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F186300001042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001042FF7FE07F1863E07F1863E07F
      1863E07F1863E07FE07F000010421042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001042FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7F1042104218631042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001042104210421042104210421042
      104210421042104210421042E07F1042000000001042FF7F1863E07F1863E07F
      1863E07F1863E07F1863E07F1863104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F1042000000001042FF7FE07F1863E07F1863
      E07F1863E07F1863E07F1863E07F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7F1863E07F1863E07F
      1863FF7FFF7FFF7FFF7FFF7FFF7F1042000000001042FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001042FF7FE07F1863E07F1863
      E07FFF7F104210421042104210421042000000001042E07F1863E07F1863E07F
      1863E07F10421042104210421042104200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001042FF7FFF7FFF7FFF7F
      FF7F10420000000000000000000000000000000000001042E07F1863E07F1863
      E07F104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000001042104210421042
      1042000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010401040
      1040104010401040104010401040104000000000000000000000000000001040
      0000104200000000000000000000000000000000000000000000000010401040
      1040104010401040104010401040104000000000000000000000000000001040
      00001042000000000000000000000000000000000000000000001040FF7F0000
      104210421863FF7FFF7FFF7FFF7F104000000000000000000000000010401040
      0000FF7F1863FF7F0000000000000000000000000000000000001040FF7F0000
      104210421863FF7FFF7FFF7FFF7F104000000000000000000000000010401040
      0000FF7F1863FF7F0000000000000000000000000000000010401040FF7F0000
      1863FF7F104210421863FF7FFF7F104000000000000000000000104010401040
      00001042FF7F1863FF7F186300000000000000000000000010401040FF7F0000
      1863FF7F104210421863FF7FFF7F104000000000000000000000104010401040
      00001042FF7F1863FF7F18630000000000000000000010401040FF7FFF7F0000
      1863FF7FFF7FFF7F10421863FF7F104000000000000000001040104010401040
      18630000000010421863FF7F1863000000000000000010401040FF7FFF7F0000
      1863FF7FFF7FFF7F10421863FF7F104000000000000000001040104010401040
      18630000000010421863FF7F186300000000000000001040FF7FFF7FFF7F0000
      1863FF7FE07FFF7FFF7F1042FF7F104000000000000010401040104010401863
      104010401040000000001042FF7F10420000000000001040FF7FFF7FFF7F0000
      1863FF7FE07FFF7FFF7F1042FF7F104000000000000010401040104010401863
      104010401040000000001042FF7F10420000000000001040FF7FFF7FFF7F0000
      1863FF7FFF7FFF7FE07F1042FF7F104000000000104010401040104018631040
      104010401040104010400000000010420000000000001040FF7FFF7FFF7F0000
      1863FF7FFF7FFF7FE07F1042FF7F104000000000104010401040104018631040
      104010401040104010400000000010420000000000001040FF7FFF7FFF7F0000
      1863FF7FE07FFF7FFF7F1042FF7F104000000000104010401040186310401040
      10401040104010401040104010400000000000000000007CFF7FFF7F007C0000
      1863FF7FE07FFF7FFF7F1042FF7F1040000000001040007C10401863007C1040
      104010401040104010401040104000000000000000001040FF7FFF7FFF7F0000
      1863FF7FFF7FFF7FE07F1042FF7F104000000000104010401863104010401040
      10401040104010401040104010400000000000000000007C007CFF7F007C0000
      1863FF7FFF7FFF7FE07F1042FF7F1040000000001040007C007C1040007C1040
      104010401040104010401040104000000000000000001040FF7FFF7FFF7F0000
      1863FF7FE07FFF7FFF7F1042FF7F104000000000104018631040104010401040
      104010401040104010401040000000000000000000001040007C007C007C0000
      007C007CE07FFF7FFF7F1042FF7F10400000000010401863007C007C007C1040
      007C007C1040104010401040000000000000000000001040FF7FFF7F18631042
      1042FF7FFF7FFF7FE07F10420000000000000000186310401040104010401040
      104010401040104010400000000000000000007C007C007C007C007C007C007C
      007CFF7FFF7FFF7FE07F1042000000000000007C007C007C007C007C007C007C
      007C10401040104010400000000000000000000000001040FF7FFF7F10420000
      000010421042FF7FFF7F10420000000000000000000000001040104010401040
      10401040104010400000000000000000000000000000007C007CFF7F007C007C
      000010421042FF7FFF7F104200000000000000000000007C007CFF7F007C007C
      104010401040104000000000000000000000000000001040FF7F186310420000
      0000000000001042104210420000000000000000000000000000000010401040
      1040104010400000000000000000000000000000007C007C007C007C007C007C
      007C007C00001042104210420000000000000000007C007C007C007C007C007C
      007C007C10400000000000000000000000000000000010421863104200000000
      0000000000000000000000000000000000000000000000000000000000000000
      104010400000000000000000000000000000007C007C1042007C007C007C0000
      000000000000000000000000000000000000007C007C0000007C007C007C0000
      1040104000000000000000000000000000000000000000001042000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000007C0000007C007C
      000000000000000000000000000000000000000000000000007C0000007C007C
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFF000000000000F9FF000000000000
      F6CF000000000000F6B7000000000000F6B7000000000000F8B7000000000000
      FE8F000000000000FE3F000000000000FF7F000000000000FE3F000000000000
      FEBF000000000000FC9F000000000000FDDF000000000000FDDF000000000000
      FDDF000000000000FFFF000000000000FFFFFFFFFFFFFFFFE000C000FFFFFFFF
      C0008000FFFFFFFFC0008000E003E00380008000EFFBEFFB80008000EEFBEFFB
      00008000EC7BEFFB00008000E83BEFFB00008000E91BEFFB80008000EB8BEFFB
      80008000EFCBEFFB80018001EFEBEFFBC07FC07FEFFBEFFBE0FFE0FFE003E003
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800FC7FF800FC7F
      F000F81FF000F81FE000F007E000F007C000E001C000E0018000C0008000C000
      8000800180008001800000018000000180000000800000008000000180000001
      800000038000000380010007000100078307800F8107800F83C7E01F8047801F
      C7FFF83F03FF203FEFFFFE7FE9FFE87F00000000000000000000000000000000
      000000000000}
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 205
    Top = 8
    object JvRollOutAction1: TJvRollOutAction
      Caption = 'Action &1 (Ctrl+1)'
      ShortCut = 16433
      OnExecute = JvRollOutAction1Execute
      RollOut = RO1
      LinkCheckedToCollapsed = False
    end
    object JvRollOutAction2: TJvRollOutAction
      Caption = 'Action &2 (Ctrl+2)'
      ShortCut = 16434
      OnExecute = JvRollOutAction2Execute
      RollOut = RO2
      LinkCheckedToCollapsed = True
    end
  end
  object MainMenu1: TMainMenu
    Left = 317
    Top = 80
    object Actions1: TMenuItem
      Caption = 'Actions'
      object Action1Ctrl11: TMenuItem
        Action = JvRollOutAction1
      end
      object Action2Ctrl21: TMenuItem
        Action = JvRollOutAction2
      end
    end
  end
end
