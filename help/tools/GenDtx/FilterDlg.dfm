object frmFilter: TfrmFilter
  Left = 264
  Top = 113
  Width = 663
  Height = 509
  Caption = 'Filter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 427
    Width = 652
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 16
    Top = 32
    Width = 161
    Height = 81
  end
  object lblMore_Property: TJvLabel
    Left = 142
    Top = 40
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object lblMore_MethodFunction: TJvLabel
    Left = 142
    Top = 64
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object lblMore_MethodProcedure: TJvLabel
    Left = 142
    Top = 88
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object lblMore_Function: TJvLabel
    Left = 142
    Top = 144
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object lblMore_Procedure: TJvLabel
    Left = 142
    Top = 192
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object jlbMore_Class: TJvLabel
    Left = 142
    Top = 8
    Width = 33
    Height = 19
    AutoSize = False
    Caption = 'More'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    Layout = tlCenter
    ParentFont = False
    OnClick = OnMoreClick
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = -1
  end
  object chbClasses: TCheckBox
    Left = 16
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Class'
    TabOrder = 0
    OnEnter = chbClassesEnter
  end
  object chbConst: TCheckBox
    Left = 192
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Const'
    TabOrder = 1
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbDispInterface: TCheckBox
    Left = 192
    Top = 8
    Width = 97
    Height = 17
    Caption = 'DispInterface'
    TabOrder = 2
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbFunction: TCheckBox
    Left = 16
    Top = 144
    Width = 97
    Height = 17
    Caption = 'Function'
    TabOrder = 3
    OnEnter = chbFunctionEnter
  end
  object chbFunctionType: TCheckBox
    Left = 16
    Top = 168
    Width = 97
    Height = 17
    Caption = 'FunctionType'
    TabOrder = 4
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbInterface: TCheckBox
    Left = 192
    Top = 32
    Width = 97
    Height = 17
    Caption = 'Interface'
    TabOrder = 5
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbMethodFunc: TCheckBox
    Left = 24
    Top = 64
    Width = 105
    Height = 17
    Caption = 'Function (method)'
    TabOrder = 6
    OnEnter = chbMethodFuncEnter
  end
  object chbMethodProc: TCheckBox
    Left = 24
    Top = 88
    Width = 113
    Height = 17
    Caption = 'Procedure (method)'
    TabOrder = 7
    OnEnter = chbMethodProcEnter
  end
  object chbProcedure: TCheckBox
    Left = 16
    Top = 192
    Width = 81
    Height = 17
    Caption = 'Procedure'
    TabOrder = 8
    OnEnter = chbProcedureEnter
  end
  object chbProcedureType: TCheckBox
    Left = 16
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Procedure Type'
    TabOrder = 9
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbProperty: TCheckBox
    Left = 24
    Top = 40
    Width = 73
    Height = 17
    Caption = 'Property'
    TabOrder = 10
    OnEnter = chbPropertyEnter
  end
  object chbRecord: TCheckBox
    Left = 192
    Top = 168
    Width = 97
    Height = 17
    Caption = 'Record'
    TabOrder = 11
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbResourcestring: TCheckBox
    Left = 192
    Top = 192
    Width = 97
    Height = 17
    Caption = 'Resourcestring'
    TabOrder = 12
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbEnum: TCheckBox
    Left = 192
    Top = 144
    Width = 97
    Height = 17
    Caption = 'Enumerate'
    TabOrder = 13
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbType: TCheckBox
    Left = 192
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Type'
    TabOrder = 14
    OnEnter = ItemWithoutMorePageEnter
  end
  object chbVar: TCheckBox
    Left = 192
    Top = 120
    Width = 97
    Height = 17
    Caption = 'Variable'
    TabOrder = 15
    OnEnter = ItemWithoutMorePageEnter
  end
  object Button1: TButton
    Left = 482
    Top = 442
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 16
  end
  object Button2: TButton
    Left = 570
    Top = 442
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 17
  end
  object rgrDuplicatesOrUnique: TRadioGroup
    Left = 312
    Top = 3
    Width = 201
    Height = 110
    Anchors = [akLeft, akBottom]
    Caption = ' Show Duplicates? '
    TabOrder = 18
  end
  object pgcMore: TPageControl
    Left = 8
    Top = 243
    Width = 641
    Height = 185
    ActivePage = tshFunction
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 19
    object tshProperty: TTabSheet
      Caption = 'tshProperty'
      TabVisible = False
      object jlbProperty_Inherited: TJvLabel
        Left = 8
        Top = 128
        Width = 209
        Height = 19
        AutoSize = False
        Caption = 'Inherited: property Visible;'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object jlbProperty_Array: TJvLabel
        Left = 8
        Top = 146
        Width = 273
        Height = 23
        AutoSize = False
        Caption = 'Array: property Item[const Index: Integer] of string etc.'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object Bevel1: TBevel
        Left = 8
        Top = 8
        Width = 209
        Height = 113
      end
      object lblProperty_SpecifierIndex: TJvLabel
        Left = 24
        Top = 40
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Index'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierRead: TJvLabel
        Left = 24
        Top = 58
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Read'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierWrite: TJvLabel
        Left = 24
        Top = 76
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Write'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierStored: TJvLabel
        Left = 24
        Top = 94
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Stored'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierDefault: TJvLabel
        Left = 104
        Top = 40
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Default'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierNoDefault: TJvLabel
        Left = 104
        Top = 58
        Width = 73
        Height = 19
        AutoSize = False
        Caption = 'Nodefault'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object lblProperty_SpecifierImplements: TJvLabel
        Left = 104
        Top = 76
        Width = 89
        Height = 19
        AutoSize = False
        Caption = 'Implements'
        OnClick = FourStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 57
        Height = 13
        Caption = 'Specifiers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pnlProperty_Scope: TPanel
        Left = 224
        Top = 8
        Width = 89
        Height = 113
        BevelOuter = bvLowered
        TabOrder = 0
        object Label8: TLabel
          Left = 6
          Top = 5
          Width = 37
          Height = 13
          Caption = 'Scope'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object lsbPropertyIn: TListBox
        Left = 320
        Top = 8
        Width = 185
        Height = 105
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
      end
      object Button3: TButton
        Left = 320
        Top = 144
        Width = 75
        Height = 25
        Action = actAddIn
        TabOrder = 2
      end
      object Button4: TButton
        Left = 400
        Top = 144
        Width = 75
        Height = 25
        Action = actDeleteIn
        TabOrder = 3
      end
      object edtPropertyIn: TEdit
        Left = 320
        Top = 120
        Width = 185
        Height = 21
        TabOrder = 4
        Text = 'edtPropertyIn'
      end
    end
    object tshProcedureMethod: TTabSheet
      Caption = 'tshProcedureMethod'
      ImageIndex = 1
      TabVisible = False
      object jlbProcedureMethod_ClassMethod: TJvLabel
        Left = 8
        Top = 152
        Width = 97
        Height = 19
        AutoSize = False
        Caption = 'Class method'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object jlbProcedureMethod_Constructor: TJvLabel
        Left = 104
        Top = 152
        Width = 97
        Height = 19
        AutoSize = False
        Caption = 'Constructor'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object jlbProcedureMethod_Destructor: TJvLabel
        Left = 192
        Top = 152
        Width = 97
        Height = 19
        AutoSize = False
        Caption = 'Destructor'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object pnlProcedureMethod_Directives: TPanel
        Left = 8
        Top = 8
        Width = 385
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 0
        object Label2: TLabel
          Left = 6
          Top = 5
          Width = 58
          Height = 13
          Caption = 'Directives'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object pnlProcedureMethod_Scope: TPanel
        Left = 400
        Top = 8
        Width = 89
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 1
        object Label7: TLabel
          Left = 6
          Top = 5
          Width = 37
          Height = 13
          Caption = 'Scope'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object chbProcMeth_MinParamCount: TCheckBox
        Left = 496
        Top = 8
        Width = 121
        Height = 17
        Caption = 'Parameter count >='
        TabOrder = 2
      end
      object sedProcMeth_MinParamCount: TJvSpinEdit
        Left = 512
        Top = 24
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 3
      end
      object chbProcMeth_MaxParamCount: TCheckBox
        Left = 496
        Top = 48
        Width = 121
        Height = 17
        Caption = 'Parameter count <='
        TabOrder = 4
      end
      object sedProcMeth_MaxParamCount: TJvSpinEdit
        Left = 512
        Top = 64
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 5
      end
    end
    object tshFunctionMethod: TTabSheet
      Caption = 'tshFunctionMethod'
      ImageIndex = 2
      TabVisible = False
      object jlbFunctionMethod_ClassMethod: TJvLabel
        Left = 8
        Top = 152
        Width = 97
        Height = 19
        AutoSize = False
        Caption = 'Class method'
        OnClick = TriStateLabelClick
        AutoOpenURL = False
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'MS Sans Serif'
        HotTrackFont.Style = []
        Images = ImageList1
        ImageIndex = 2
      end
      object pnlFunctionMethod_Scope: TPanel
        Left = 392
        Top = 8
        Width = 89
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 0
        object Label4: TLabel
          Left = 6
          Top = 5
          Width = 37
          Height = 13
          Caption = 'Scope'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object pnlFunctionMethod_Directives: TPanel
        Left = 8
        Top = 8
        Width = 377
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 1
        object Label3: TLabel
          Left = 6
          Top = 5
          Width = 58
          Height = 13
          Caption = 'Directives'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object CheckBox5: TCheckBox
        Left = 488
        Top = 8
        Width = 121
        Height = 17
        Caption = 'Parameter count >='
        TabOrder = 2
      end
      object CheckBox6: TCheckBox
        Left = 488
        Top = 48
        Width = 105
        Height = 17
        Caption = 'Parameter count <='
        TabOrder = 3
      end
      object JvSpinEdit5: TJvSpinEdit
        Left = 504
        Top = 64
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 4
      end
      object JvSpinEdit6: TJvSpinEdit
        Left = 504
        Top = 24
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 5
      end
    end
    object tshNone: TTabSheet
      Caption = 'tshNone'
      ImageIndex = 3
      TabVisible = False
    end
    object tshProcedure: TTabSheet
      Caption = 'tshProcedure'
      ImageIndex = 4
      TabVisible = False
      object pnlProcedure_Directives: TPanel
        Left = 8
        Top = 8
        Width = 417
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 0
        object Label5: TLabel
          Left = 6
          Top = 5
          Width = 58
          Height = 13
          Caption = 'Directives'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object CheckBox3: TCheckBox
        Left = 432
        Top = 8
        Width = 121
        Height = 17
        Caption = 'Parameter count >='
        TabOrder = 1
      end
      object CheckBox4: TCheckBox
        Left = 432
        Top = 32
        Width = 121
        Height = 17
        Caption = 'Parameter count <='
        TabOrder = 2
      end
      object JvSpinEdit3: TJvSpinEdit
        Left = 560
        Top = 32
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 3
      end
      object JvSpinEdit4: TJvSpinEdit
        Left = 560
        Top = 8
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 4
      end
    end
    object tshClass: TTabSheet
      Caption = 'tshClass'
      ImageIndex = 6
      TabVisible = False
      object Label9: TLabel
        Left = 8
        Top = 16
        Width = 73
        Height = 13
        Caption = 'Descendant of:'
      end
      object edtAncestor: TEdit
        Left = 96
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'edtAncestor'
      end
    end
    object tshFunction: TTabSheet
      Caption = 'tshFunction'
      ImageIndex = 5
      TabVisible = False
      object pnlFunction_Directives: TPanel
        Left = 8
        Top = 8
        Width = 417
        Height = 137
        BevelOuter = bvLowered
        TabOrder = 0
        object Label6: TLabel
          Left = 6
          Top = 5
          Width = 58
          Height = 13
          Caption = 'Directives'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object JvSpinEdit1: TJvSpinEdit
        Left = 560
        Top = 8
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 1
      end
      object JvSpinEdit2: TJvSpinEdit
        Left = 560
        Top = 32
        Width = 57
        Height = 21
        CheckOptions = [coCheckOnExit, coCropBeyondLimit]
        ButtonKind = bkClassic
        MaxValue = 100.000000000000000000
        TabOrder = 2
      end
      object CheckBox1: TCheckBox
        Left = 432
        Top = 8
        Width = 121
        Height = 17
        Caption = 'Parameter count >='
        TabOrder = 3
      end
      object CheckBox2: TCheckBox
        Left = 432
        Top = 32
        Width = 121
        Height = 17
        Caption = 'Parameter count <='
        TabOrder = 4
      end
    end
  end
  object grbLegend: TGroupBox
    Left = 520
    Top = 8
    Width = 129
    Height = 97
    Caption = ' Meaning: '
    TabOrder = 20
    object JvLabel1: TJvLabel
      Left = 8
      Top = 16
      Width = 113
      Height = 19
      AutoSize = False
      Caption = 'Must not be set'
      AutoOpenURL = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Images = ImageList1
      ImageIndex = 0
    end
    object JvLabel2: TJvLabel
      Left = 8
      Top = 36
      Width = 89
      Height = 19
      AutoSize = False
      Caption = 'Don'#39't care'
      AutoOpenURL = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Images = ImageList1
      ImageIndex = 1
    end
    object JvLabel3: TJvLabel
      Left = 8
      Top = 56
      Width = 116
      Height = 19
      AutoSize = False
      Caption = 'One of must be set'
      AutoOpenURL = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Images = ImageList1
      ImageIndex = 2
    end
    object JvLabel4: TJvLabel
      Left = 8
      Top = 76
      Width = 101
      Height = 19
      AutoSize = False
      Caption = 'Must be set'
      AutoOpenURL = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      Images = ImageList1
      ImageIndex = 3
    end
  end
  object rgrSearchSection: TRadioGroup
    Left = 312
    Top = 123
    Width = 201
    Height = 110
    Anchors = [akLeft, akBottom]
    Caption = ' Search where? '
    TabOrder = 21
  end
  object chbSearchInterface: TCheckBox
    Left = 328
    Top = 144
    Width = 129
    Height = 17
    Caption = 'Interface section'
    TabOrder = 22
  end
  object chbSearchImplementation: TCheckBox
    Left = 328
    Top = 168
    Width = 153
    Height = 17
    Caption = 'Implementation section'
    TabOrder = 23
  end
  object ActionList1: TActionList
    Left = 520
    Top = 8
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actAddIn: TAction
      Category = 'Property'
      Caption = 'Add'
      OnExecute = actAddInExecute
      OnUpdate = actAddInUpdate
    end
    object actDeleteIn: TAction
      Category = 'Property'
      Caption = 'Delete'
      OnExecute = actDeleteInExecute
      OnUpdate = actDeleteInUpdate
    end
  end
  object ImageList1: TImageList
    Left = 552
    Top = 8
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
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
      000000000000000000000000000000000000000000009C8C6F009C8C6F000000
      0000000000009C8C6F009C8C6F0000000000000000009C8C6F009C8C6F009C8C
      6F009C8C6F009C8C6F009C8C6F009C8C6F009C8C6F009C8C6F009C8C6F009C8C
      6F009C8C6F009C8C6F009C8C6F009C8C6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009C8C
      6F009C8C6F009C8C6F00000000000000000000000000000000009C8C6F009C8C
      6F00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C8C6F009C8C6F000000
      00009C8C6F00000000009C8C6F009C8C6F009C8C6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C8C6F009C8C6F000000000000000000000000000000
      00009C8C6F009C8C6F009C8C6F00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C8C6F000000
      0000000000009C8C6F009C8C6F0000000000000000009C8C6F00000000000000
      00009C8C6F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF00000000009C8C6F009C8C
      6F00000000000000FF00000000009C8C6F0000000000FFFFFF00C0928F00C092
      8F00C0928F00C0928F00C0928F00C0928F00C0928F00C0928F00C0928F00C092
      8F00C0928F00C0928F00000000009C8C6F000000000000000000000000009C8C
      6F0000000000000000009C8C6F009C8C6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      FF0000009900000000009C8C6F009C8C6F009C8C6F0000000000000099000000
      9900000000009C8C6F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009999FF000000FF00000000000000
      00000000FF006666FF00000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900000000000000000000000000000000000000000000000000C092
      8F00BFBFBF00C0928F00000000009C8C6F000000000000000000000000000000
      00000C4D24000C4D2400000000009C8C6F009C8C6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009999
      FF000000FF0000009900000000009C8C6F00000000002600C4000000FF000000
      9900000000009C8C6F0000000000000000000000000000000000000000000000
      00000000000000000000000000009C8C6F00000000009999FF000000FF000000
      FF006666FF0000000000000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000C0928F00BFBFBF00F9EE
      D900F9EED900C0928F00000000009C8C6F0000000000000000009C8C6F000000
      000019A64D0019A64D000C4D2400000000009C8C6F009C8C6F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009999FF000000FF0000009900000000002600C4000000FF002600C4000000
      00009C8C6F000000000000000000000000000000000000000000000000009C8C
      6F009C8C6F000000000000000000000000009C8C6F00000000006666FF006666
      FF00000000009C8C6F009C8C6F000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000C0928F00BFBFBF00F9EE
      D900F9EED900C0928F00000000009C8C6F0000000000000000000000000019A6
      4D0019A64D0000C0920019A64D000C4D2400000000009C8C6F009C8C6F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009999FF000000FF002600C4000000FF002600C400000000009C8C
      6F000000000000000000000000000000000000000000000000009C8C6F000000
      00009C8C6F009C8C6F00000000009C8C6F00000000000000FF009999FF009999
      FF000000FF00000000009C8C6F009C8C6F0000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000C0928F00BFBFBF00F9EE
      D900F9EED900C0928F00000000009C8C6F00000000009C8C6F000000000019A6
      4D0000C092000000000000C0920019A64D000C4D2400000000009C8C6F009C8C
      6F00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF00000000009C8C6F009C8C
      6F0000000000000000000000000000000000000000009C8C6F000000000019A6
      4D00000000009C8C6F009C8C6F00000000000000FF006666FF00000000000000
      00009999FF000000FF00000000009C8C6F0000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000C0928F00BFBFBF00F9EE
      D900F9EED900C0928F00000000009C8C6F00000000000000000019A64D0000C0
      920000000000000000000000000000C0920019A64D000C4D2400000000009C8C
      6F009C8C6F000000000000000000000000000000000000000000000000000000
      0000000000002600C4000000FF009999FF000000FF0000009900000000009C8C
      6F009C8C6F000000000000000000000000009C8C6F000000000019A64D0000FF
      000019A64D00000000009C8C6F0000000000CCCCFF00000000009C8C6F009C8C
      6F00000000009999FF00000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000C0928F00BFBFBF00F9EE
      D900F9EED900C0928F00000000009C8C6F00000000000000000019A64D0019A6
      4D000000000000000000000000000000000000C0920019A64D000C4D24000000
      00009C8C6F009C8C6F0000000000000000000000000000000000000000000000
      00002600C4000000FF002600C400000000009999FF000000FF00000099000000
      00009C8C6F009C8C6F0000000000000000000000000019A64D0000FF00000000
      000000FF000019A64D00000000009C8C6F00000000009C8C6F00000000000000
      00009C8C6F0000000000000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900BFBFBF000000000000000000C0928F00F9EED900F9EE
      D900F9EED900C0928F00000000009C8C6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000C0920019A64D000C4D
      2400000000009C8C6F009C8C6F00000000000000000000000000000000009999
      FF000000FF002600C4000000000000000000000000009999FF000000FF000000
      9900000000009C8C6F00000000000000000000000000D6FFFF00000000009C8C
      6F000000000000FF000019A64D00000000009C8C6F009C8C6F00000000000000
      00000000000000000000000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED90000000000000000000000000000000000C0928F00F9EED900F9EE
      D900F9EED900C0928F00000000009C8C6F000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000C0920019A6
      4D000C4D2400000000009C8C6F00000000000000000000000000000000009999
      FF009999FF0000000000000000000000000000000000000000009999FF009999
      FF00000000009C8C6F00000000000000000000000000000000009C8C6F000000
      00009C8C6F000000000000FF000019A64D00000000009C8C6F009C8C6F000000
      00000000000000000000000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED9000000000000000000F9EED900F9EED900F9EE
      D900F9EED900C0928F00000000009C8C6F000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000C0
      920019A64D000C4D240000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00009C8C6F000000000000000000000000000000000000000000000000000000
      0000000000009C8C6F000000000000FF000019A64D00000000009C8C6F000000
      00000000000000000000000000000000000000000000FFFFFF00F9EED900F9EE
      D900F9EED900F9EED900F9EED900F9EED900F9EED900F9EED900F9EED900F9EE
      D900F9EED900C0928F00000000009C8C6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000C0920019A64D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C8C6F000000000000FF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000009C8C6F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009C8C6F00000000009C8C6F00000000000000
      0000000000000000000000000000000000009C8C6F0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000009C8C6F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFF998000FFFF
      E3CFFF100000F1FFC187FE000000E0FFC003FE010000E07FC003FE030000C03F
      E007E7010000C01FF00FC2000000800FF80F800000008007F007000100008603
      E00300330000CF01C103003F0000FF81C383901F0000FFC1E7C7F81F0000FFE1
      FFFFFC3F0000FFF3FFFFFE3F0001FFFF00000000000000000000000000000000
      000000000000}
  end
end
