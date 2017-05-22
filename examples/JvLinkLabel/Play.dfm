object frmPlay: TfrmPlay
  Left = 252
  Top = 135
  Width = 633
  Height = 567
  BorderIcons = [biSystemMenu]
  Caption = 'Play with the TJvLinkLabel!'
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LinkLabel: TJvLinkLabel
    Left = 0
    Top = 0
    Width = 330
    Height = 496
    Transparent = False
    Layout = tlTop
    LinkColor = clBlue
    LinkColorClicked = clRed
    LinkColorHot = clLime
    LinkStyle = []
    HotLinks = True
    AutoHeight = True
    MarginWidth = 0
    MarginHeight = 0
    OnLinkClick = LinkLabelLinkClick
    Align = alClient
    Color = 8454143
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnMouseMove = LinkLabelMouseMove
  end
  object Splitter1: TSplitter
    Left = 330
    Top = 0
    Width = 9
    Height = 496
    Cursor = crHSplit
    Align = alRight
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 339
    Top = 0
    Width = 286
    Height = 496
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 286
      Height = 125
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btnRefresh: TButton
        Left = 4
        Top = 100
        Width = 277
        Height = 21
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshClick
      end
      object rgTextLayout: TRadioGroup
        Left = 4
        Top = 4
        Width = 93
        Height = 93
        Caption = ' Vert Alignment '
        Items.Strings = (
          'Top'
          'Center'
          'Bottom')
        TabOrder = 1
        OnClick = rgTextLayoutClick
      end
      object gbMargin: TGroupBox
        Left = 100
        Top = 4
        Width = 89
        Height = 93
        Caption = ' Margin '
        TabOrder = 2
        object Label1: TLabel
          Left = 12
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Width'
        end
        object Label2: TLabel
          Left = 12
          Top = 52
          Width = 31
          Height = 13
          Caption = 'Height'
        end
        object edMarginW: TJvValidateEdit
          Left = 12
          Top = 28
          Width = 49
          Height = 21
          CheckChars = '01234567890'
          CriticalPoints.CheckPoints = cpNone
          CriticalPoints.ColorAbove = clBlue
          CriticalPoints.ColorBelow = clRed
          EditText = '0'
          PasswordChar = #0
          TabOrder = 0
          Text = '0'
          Value = 0
          OnChange = edMarginWChange
        end
        object edMarginH: TJvValidateEdit
          Left = 12
          Top = 64
          Width = 49
          Height = 21
          CheckChars = '01234567890'
          CriticalPoints.CheckPoints = cpNone
          CriticalPoints.ColorAbove = clBlue
          CriticalPoints.ColorBelow = clRed
          EditText = '0'
          PasswordChar = #0
          TabOrder = 1
          Text = '0'
          Value = 0
          OnChange = edMarginHChange
        end
        object updnW: TUpDown
          Left = 61
          Top = 28
          Width = 15
          Height = 21
          Associate = edMarginW
          Min = 0
          Position = 0
          TabOrder = 2
          Wrap = False
        end
        object updbH: TUpDown
          Left = 61
          Top = 64
          Width = 15
          Height = 21
          Associate = edMarginH
          Min = 0
          Position = 0
          TabOrder = 3
          Wrap = False
        end
      end
      object gbMousePos: TGroupBox
        Left = 192
        Top = 4
        Width = 89
        Height = 93
        Caption = ' Mouse Pos '
        TabOrder = 3
        object Label4: TLabel
          Left = 5
          Top = 16
          Width = 76
          Height = 13
          Alignment = taCenter
          Caption = 'TLinkLabel (X,Y)'
        end
        object LnkLblMousePos: TLabel
          Left = 5
          Top = 32
          Width = 80
          Height = 14
          Alignment = taCenter
          AutoSize = False
          Caption = 'Memo (X,Y)'
        end
      end
    end
    object TreeView: TTreeView
      Left = 0
      Top = 125
      Width = 286
      Height = 371
      Align = alClient
      Indent = 19
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 496
    Width = 625
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 625
      Height = 2
      Align = alTop
    end
    object Edit: TEdit
      Left = 8
      Top = 8
      Width = 613
      Height = 21
      TabOrder = 0
      OnChange = EditChange
    end
  end
end
