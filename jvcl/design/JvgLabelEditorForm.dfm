inherited JvgLabelEditorDlg: TJvgLabelEditorDlg
  Left = 336
  Top = 135
  Width = 566
  Height = 588
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnMain: TPanel
    Width = 558
    Height = 508
    object splJvgSplitter1: TSplitter [0]
      Left = 1
      Top = 81
      Width = 556
      Height = 8
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      Color = clBtnFace
      ParentColor = False
    end
    inherited pgMain: TPageControl
      Top = 89
      Width = 556
      Height = 418
      inherited tabMain: TTabSheet
        object JvgGroupBox2: TGroupBox
          Left = 6
          Top = 14
          Width = 192
          Height = 267
          Caption = 'Colors'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object JvgLabel20: TLabel
            Left = 72
            Top = 20
            Width = 21
            Height = 14
            Anchors = []
            Caption = 'Text'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel19: TLabel
            Left = 39
            Top = 43
            Width = 55
            Height = 14
            Anchors = []
            Caption = 'Text Active'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel18: TLabel
            Left = 29
            Top = 65
            Width = 65
            Height = 14
            Anchors = []
            Caption = 'Text Disabled'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel17: TLabel
            Left = 55
            Top = 100
            Width = 40
            Height = 14
            Caption = 'Highlight'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel16: TLabel
            Left = 57
            Top = 124
            Width = 41
            Height = 14
            Caption = 'Shadow'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel15: TLabel
            Left = 40
            Top = 154
            Width = 58
            Height = 14
            Caption = 'Background'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel14: TLabel
            Left = 6
            Top = 178
            Width = 92
            Height = 14
            Caption = 'Background Active'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel13: TLabel
            Left = 54
            Top = 213
            Width = 44
            Height = 14
            Caption = 'Delineate'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel10: TLabel
            Left = 23
            Top = 237
            Width = 75
            Height = 14
            Caption = 'DelineateActive'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object cbtnText: TComboBox
            Tag = 1
            Left = 106
            Top = 15
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cLabelColorsChange
          end
          object cbtnTextActive: TComboBox
            Tag = 2
            Left = 106
            Top = 39
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = cLabelColorsChange
          end
          object cbtnTextDisabled: TComboBox
            Tag = 3
            Left = 106
            Top = 63
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
            OnChange = cLabelColorsChange
          end
          object cbtnHighlight: TComboBox
            Tag = 4
            Left = 106
            Top = 95
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 3
            OnChange = cLabelColorsChange
          end
          object cbtnShadow: TComboBox
            Tag = 5
            Left = 106
            Top = 119
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 4
            OnChange = cLabelColorsChange
          end
          object cbtnBackground: TComboBox
            Tag = 6
            Left = 106
            Top = 151
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 5
            OnChange = cLabelColorsChange
          end
          object cbtnBackgroundActive: TComboBox
            Tag = 7
            Left = 106
            Top = 175
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 6
            OnChange = cLabelColorsChange
          end
          object cbtnDelineate: TComboBox
            Tag = 8
            Left = 106
            Top = 209
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 7
            OnChange = cLabelColorsChange
          end
          object cbtnDelineateActive: TComboBox
            Tag = 9
            Left = 106
            Top = 233
            Width = 73
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 8
            OnChange = cLabelColorsChange
          end
        end
        object gbxPassiveStyle: TGroupBox
          Left = 329
          Top = 17
          Width = 97
          Height = 145
          Caption = 'Passive'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object cbxPassiveNormal: TCheckBox
            Left = 10
            Top = 24
            Width = 80
            Height = 17
            Caption = 'Normal'
            TabOrder = 0
            OnClick = cbxPassiveNormalClick
          end
          object cbxPassivePushed: TCheckBox
            Tag = 1
            Left = 10
            Top = 43
            Width = 80
            Height = 17
            Caption = 'Pushed'
            TabOrder = 1
            OnClick = cbxPassiveNormalClick
          end
          object cbxPassiveRaised: TCheckBox
            Tag = 2
            Left = 10
            Top = 62
            Width = 80
            Height = 17
            Caption = 'Raised'
            TabOrder = 2
            OnClick = cbxPassiveNormalClick
          end
          object cbxPassiveShadow: TCheckBox
            Tag = 3
            Left = 10
            Top = 82
            Width = 80
            Height = 17
            Caption = 'Shadow'
            TabOrder = 3
            OnClick = cbxPassiveNormalClick
          end
          object cbxPassiveRecessed: TCheckBox
            Tag = 4
            Left = 10
            Top = 101
            Width = 80
            Height = 17
            Caption = 'Recessed'
            TabOrder = 4
            OnClick = cbxPassiveNormalClick
          end
          object cbxPassiveVolumentric: TCheckBox
            Tag = 5
            Left = 10
            Top = 120
            Width = 80
            Height = 17
            Caption = 'Volumetric'
            TabOrder = 5
            OnClick = cbxPassiveNormalClick
          end
        end
        object gbxActiveStyle: TGroupBox
          Left = 214
          Top = 17
          Width = 97
          Height = 145
          Caption = 'Active'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          object cbxActiveNormal: TCheckBox
            Left = 10
            Top = 24
            Width = 80
            Height = 17
            Caption = 'Normal'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbxActiveNormalClick
          end
          object cbxActivePushed: TCheckBox
            Tag = 1
            Left = 10
            Top = 43
            Width = 80
            Height = 17
            Caption = 'Pushed'
            TabOrder = 1
            OnClick = cbxActiveNormalClick
          end
          object cbxActiveRaised: TCheckBox
            Tag = 2
            Left = 10
            Top = 62
            Width = 80
            Height = 17
            Caption = 'Raised'
            TabOrder = 2
            OnClick = cbxActiveNormalClick
          end
          object cbxActiveShadow: TCheckBox
            Tag = 3
            Left = 10
            Top = 82
            Width = 80
            Height = 17
            Caption = 'Shadow'
            TabOrder = 3
            OnClick = cbxActiveNormalClick
          end
          object cbxActiveRecessed: TCheckBox
            Tag = 4
            Left = 10
            Top = 101
            Width = 80
            Height = 17
            Caption = 'Recessed'
            TabOrder = 4
            OnClick = cbxActiveNormalClick
          end
          object cbxActiveVolumetric: TCheckBox
            Tag = 5
            Left = 10
            Top = 120
            Width = 80
            Height = 17
            Caption = 'Volumetric'
            TabOrder = 5
            OnClick = cbxActiveNormalClick
          end
        end
        object gbxDisabledStyle: TGroupBox
          Left = 440
          Top = 17
          Width = 97
          Height = 145
          Caption = 'Disabled'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          object cbxDisabledNormal: TCheckBox
            Left = 10
            Top = 24
            Width = 80
            Height = 17
            Caption = 'Normal'
            TabOrder = 0
            OnClick = cbxDisabledNormalClick
          end
          object cbxDisabledPushed: TCheckBox
            Tag = 1
            Left = 10
            Top = 43
            Width = 80
            Height = 17
            Caption = 'Pushed'
            TabOrder = 1
            OnClick = cbxDisabledNormalClick
          end
          object cbxDisabledRaised: TCheckBox
            Tag = 2
            Left = 10
            Top = 62
            Width = 80
            Height = 17
            Caption = 'Raised'
            TabOrder = 2
            OnClick = cbxDisabledNormalClick
          end
          object cbxDisabledShadow: TCheckBox
            Tag = 3
            Left = 10
            Top = 82
            Width = 80
            Height = 17
            Caption = 'Shadow'
            TabOrder = 3
            OnClick = cbxDisabledNormalClick
          end
          object cbxDisabledRecessed: TCheckBox
            Tag = 4
            Left = 10
            Top = 101
            Width = 80
            Height = 17
            Caption = 'Recessed'
            TabOrder = 4
            OnClick = cbxDisabledNormalClick
          end
          object cbxDisabledVolumentric: TCheckBox
            Tag = 5
            Left = 10
            Top = 120
            Width = 80
            Height = 17
            Caption = 'Volumetric'
            TabOrder = 5
            OnClick = cbxDisabledNormalClick
          end
        end
        object JvgGroupBox1: TGroupBox
          Left = 333
          Top = 177
          Width = 91
          Height = 103
          Caption = 'Text Style'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          object cbxBold: TCheckBox
            Tag = 4
            Left = 11
            Top = 24
            Width = 70
            Height = 14
            Cursor = crHandPoint
            Caption = 'Bold'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            OnClick = cbxBoldClick
          end
          object cbxItalic: TCheckBox
            Tag = 4
            Left = 11
            Top = 44
            Width = 70
            Height = 14
            Cursor = crHandPoint
            Caption = 'Italic'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsItalic]
            ParentFont = False
            TabOrder = 1
            OnClick = cbxItalicClick
          end
          object cbxUnderline: TCheckBox
            Tag = 4
            Left = 11
            Top = 64
            Width = 70
            Height = 14
            Cursor = crHandPoint
            Caption = 'Underline'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = cbxUnderlineClick
          end
          object cbxStrikeOut: TCheckBox
            Tag = 4
            Left = 11
            Top = 84
            Width = 70
            Height = 14
            Cursor = crHandPoint
            Caption = 'StrikeOut'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = [fsStrikeOut]
            ParentFont = False
            TabOrder = 3
            OnClick = cbxStrikeOutClick
          end
        end
        object gbxLabelDirection: TGroupBox
          Left = 214
          Top = 177
          Width = 106
          Height = 103
          Caption = 'Direction'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          object sbtnLabelDirectionDown: TSpeedButton
            Tag = 2
            Left = 7
            Top = 48
            Width = 23
            Height = 22
            BiDiMode = bdLeftToRight
            ParentBiDiMode = False
            Spacing = -2
            OnClick = sbtnLabelDirectionRightClick
          end
          object sbtnLabelDirectionUp: TSpeedButton
            Tag = 3
            Left = 77
            Top = 48
            Width = 23
            Height = 22
            BiDiMode = bdLeftToRight
            ParentBiDiMode = False
            Spacing = -2
            OnClick = sbtnLabelDirectionRightClick
          end
          object JvgLabel5: TLabel
            Left = 32
            Top = 53
            Width = 42
            Height = 14
            Anchors = []
            Caption = 'Direction'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object sbtnLabelDirectionLeft: TSpeedButton
            Tag = 1
            Left = 40
            Top = 72
            Width = 23
            Height = 22
            BiDiMode = bdLeftToRight
            ParentBiDiMode = False
            Spacing = -2
            OnClick = sbtnLabelDirectionRightClick
          end
          object sbtnLabelDirectionRight: TSpeedButton
            Left = 40
            Top = 24
            Width = 23
            Height = 22
            BiDiMode = bdLeftToRight
            ParentBiDiMode = False
            Spacing = -2
            OnClick = sbtnLabelDirectionRightClick
          end
        end
        object JvgGroupBox4: TGroupBox
          Left = 8
          Top = 284
          Width = 417
          Height = 65
          Caption = 'Sizes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          object lblFontSize: TLabel
            Left = 377
            Top = 19
            Width = 6
            Height = 14
            Caption = '0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object lblShadowDepth: TLabel
            Left = 377
            Top = 40
            Width = 6
            Height = 14
            Caption = '0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel2: TLabel
            Left = 20
            Top = 41
            Width = 72
            Height = 14
            Caption = 'Shadow Depth'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel3: TLabel
            Left = 47
            Top = 16
            Width = 45
            Height = 14
            Caption = 'Font Size'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object sbarShadowDepth: TScrollBar
            Left = 108
            Top = 39
            Width = 254
            Height = 16
            PageSize = 0
            TabOrder = 0
            OnChange = sbarShadowDepthChange
          end
          object sbarFontSize: TScrollBar
            Left = 108
            Top = 18
            Width = 254
            Height = 16
            PageSize = 0
            TabOrder = 1
            OnChange = tbarFontSizeChange
          end
        end
        object cbxFont: TComboBox
          Left = 440
          Top = 186
          Width = 97
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 7
          OnChange = cbxFontChange
        end
      end
      object tbsTabSheet1: TTabSheet
        Caption = 'Surface'
        ImageIndex = 1
        object gbxGradient: TGroupBox
          Left = 8
          Top = 27
          Width = 257
          Height = 278
          Caption = 'Gradient'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object JvgLabel11: TLabel
            Left = 12
            Top = 171
            Width = 52
            Height = 14
            Anchors = []
            Caption = 'Orientation'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object lblGradientPercentFilling: TLabel
            Left = 235
            Top = 223
            Width = 6
            Height = 14
            Caption = '0'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel12: TLabel
            Left = 14
            Top = 205
            Width = 39
            Height = 14
            Anchors = []
            Caption = '% Filling'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel1: TLabel
            Left = 8
            Top = 79
            Width = 56
            Height = 14
            Anchors = []
            Caption = 'Brush Style'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel6: TLabel
            Left = 12
            Top = 107
            Width = 52
            Height = 14
            Anchors = []
            Caption = 'From Color'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object JvgLabel9: TLabel
            Left = 24
            Top = 129
            Width = 40
            Height = 14
            Anchors = []
            Caption = 'To Color'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object cbxGradientActive: TCheckBox
            Tag = 4
            Left = 79
            Top = 24
            Width = 106
            Height = 14
            Cursor = crHandPoint
            Caption = 'Active'
            TabOrder = 5
            OnClick = cbxGradientActiveClick
          end
          object cbxGradientBufferedDraw: TCheckBox
            Tag = 4
            Left = 79
            Top = 48
            Width = 106
            Height = 14
            Cursor = crHandPoint
            Caption = 'Buffered Draw'
            TabOrder = 6
            OnClick = cbxGradientBufferedDrawClick
          end
          object cbxGradientOrientation: TComboBox
            Tag = -1
            Left = 80
            Top = 168
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnChange = cbxGradientOrientationChange
            Items.Strings = (
              'Horizontal'
              'Vertical'
              'LeftBias'
              'RightBias'
              'Rectangle'
              'HorzConvergent'
              'VertConvergent')
          end
          object cbtnGradientColorTo: TComboBox
            Tag = 2
            Left = 80
            Top = 128
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 1
            OnChange = cbtnGradientColorFromChange
          end
          object cbtnGradientColorFrom: TComboBox
            Tag = 1
            Left = 80
            Top = 104
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 2
            OnChange = cbtnGradientColorFromChange
          end
          object sbarGradientPercentFilling: TScrollBar
            Left = 16
            Top = 224
            Width = 201
            Height = 16
            PageSize = 0
            TabOrder = 3
            OnChange = sbarGradientPercentFillingChange
          end
          object cbxBrushStyle: TComboBox
            Tag = -1
            Left = 80
            Top = 72
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 4
            Items.Strings = (
              'Solid'
              'Clear'
              'Horizontal'
              'Vertical'
              'FDiagonal'
              'BDiagonal'
              'Cross'
              'DiagCross')
          end
        end
        object JvgGroupBox3: TGroupBox
          Left = 312
          Top = 30
          Width = 193
          Height = 123
          Caption = 'Background'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object sbtnBtnNewBackgroundImage: TSpeedButton
            Left = 107
            Top = 46
            Width = 56
            Height = 25
            Cursor = crHandPoint
            Anchors = [akRight, akBottom]
            Caption = 'Open'
            Flat = True
            OnClick = btnNewTextureImageClick
          end
          object bvlBevel1: TBevel
            Left = 16
            Top = 32
            Width = 71
            Height = 67
          end
          object imgBackground: TImage
            Left = 16
            Top = 32
            Width = 73
            Height = 65
          end
          object cbxActiveBackground: TCheckBox
            Left = 111
            Top = 96
            Width = 66
            Height = 14
            Cursor = crHandPoint
            Caption = 'Active'
            TabOrder = 0
            OnClick = cbxActiveBackgroundClick
          end
        end
        object JvgGroupBox6: TGroupBox
          Left = 312
          Top = 163
          Width = 193
          Height = 142
          Caption = 'Texture'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          object sbtnBtnNewTextureImage: TSpeedButton
            Tag = 1
            Left = 108
            Top = 54
            Width = 55
            Height = 25
            Cursor = crHandPoint
            Anchors = [akRight, akBottom]
            Caption = 'Open'
            Flat = True
            OnClick = btnNewTextureImageClick
          end
          object bvlBevel2: TBevel
            Left = 16
            Top = 32
            Width = 73
            Height = 73
          end
          object imgTexture: TImage
            Left = 16
            Top = 32
            Width = 71
            Height = 72
            Picture.Data = {07544269746D617000000000}
          end
          object cbxActiveTexture: TCheckBox
            Tag = 1
            Left = 111
            Top = 104
            Width = 66
            Height = 14
            Cursor = crHandPoint
            Caption = 'Active'
            TabOrder = 0
            OnClick = cbxActiveBackgroundClick
          end
        end
      end
    end
    object pnlPanel1: TPanel
      Left = 1
      Top = 1
      Width = 556
      Height = 80
      Align = alTop
      TabOrder = 1
    end
  end
  inherited pnBottom: TPanel
    Top = 508
    Width = 558
    inherited btnCancel1: TButton
      Left = 472
    end
    inherited btnOK1: TButton
      Left = 387
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 248
    Top = 464
  end
  object JvColorDialog1: TColorDialog
    Ctl3D = True
    Left = 297
    Top = 456
  end
  object imglBrushes: TImageList
    Left = 418
    Top = 440
  end
end
