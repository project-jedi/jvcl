object frmSettings: TfrmSettings
  Left = 400
  Top = 208
  Width = 558
  Height = 370
  Caption = 'frmSettings'
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
  object pgcSettings: TPageControl
    Left = 8
    Top = 8
    Width = 534
    Height = 277
    ActivePage = tshOutput
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tshOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 1
      object tbcOutputTypes: TTabControl
        Left = 0
        Top = 0
        Width = 526
        Height = 249
        Align = alClient
        MultiLine = True
        TabOrder = 0
        Tabs.Strings = (
          'Class'
          'Class Header'
          'Const'
          'DispInterface'
          'Field'
          'Function'
          'FunctionType'
          'Header'
          'Interface'
          'Procedure'
          'ProcedureType'
          'Property'
          'Record'
          'ResourceString'
          'Set'
          'Type'
          'Var')
        TabIndex = 0
        OnChange = tbcOutputTypesChange
        object lblOutput: TLabel
          Left = 8
          Top = 64
          Width = 35
          Height = 13
          Caption = '&Output:'
          FocusControl = memOutput
        end
        object lblFor: TLabel
          Left = 365
          Top = 64
          Width = 18
          Height = 13
          Anchors = [akTop, akRight]
          Caption = '&For:'
          FocusControl = lsbFor
        end
        object memOutput: TMemo
          Left = 8
          Top = 80
          Width = 350
          Height = 157
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'memOutput')
          TabOrder = 0
        end
        object lsbFor: TListBox
          Left = 365
          Top = 80
          Width = 153
          Height = 129
          Anchors = [akTop, akRight]
          ItemHeight = 13
          Sorted = True
          TabOrder = 1
          OnClick = lsbForClick
        end
        object btnAdd: TButton
          Left = 365
          Top = 216
          Width = 75
          Height = 25
          Action = actAdd
          Anchors = [akTop, akRight]
          TabOrder = 2
        end
        object btnDelete: TButton
          Left = 445
          Top = 216
          Width = 75
          Height = 25
          Action = actDelete
          Anchors = [akTop, akRight]
          TabOrder = 3
        end
        object chbEnabled: TCheckBox
          Left = 8
          Top = 43
          Width = 97
          Height = 17
          Caption = 'Enabled'
          TabOrder = 4
        end
      end
    end
    object tshNiceNames: TTabSheet
      Caption = 'Nice Names'
      ImageIndex = 2
      object lblDefaultNiceName: TLabel
        Left = 8
        Top = 16
        Width = 37
        Height = 13
        Caption = '&Default:'
        FocusControl = edtDefaultNiceName
      end
      object lsbNiceNames: TListBox
        Left = 8
        Top = 56
        Width = 512
        Height = 155
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
      end
      object Button1: TButton
        Left = 282
        Top = 219
        Width = 75
        Height = 25
        Action = actAddNiceName
        Anchors = [akRight, akBottom]
        TabOrder = 2
      end
      object Button2: TButton
        Left = 362
        Top = 219
        Width = 75
        Height = 25
        Action = actDeleteNiceName
        Anchors = [akRight, akBottom]
        TabOrder = 3
      end
      object Button3: TButton
        Left = 442
        Top = 219
        Width = 75
        Height = 25
        Action = actEditNiceName
        Anchors = [akRight, akBottom]
        TabOrder = 4
      end
      object edtDefaultNiceName: TEdit
        Left = 64
        Top = 16
        Width = 456
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object tshDirectives: TTabSheet
      Caption = 'Directives'
      ImageIndex = 3
      object lsbDirectives: TListBox
        Left = 0
        Top = 0
        Width = 526
        Height = 212
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object Button4: TButton
        Left = 282
        Top = 219
        Width = 75
        Height = 25
        Action = actAddDirective
        Anchors = [akRight, akBottom]
        TabOrder = 1
      end
      object Button5: TButton
        Left = 362
        Top = 219
        Width = 75
        Height = 25
        Action = actDeleteDirective
        Anchors = [akRight, akBottom]
        TabOrder = 2
      end
      object edtDirective: TEdit
        Left = 8
        Top = 219
        Width = 264
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 3
      end
    end
    object tshIgnoredUnits: TTabSheet
      Caption = 'Ignored Units'
      ImageIndex = 4
      object lsbIgnoredUnits: TListBox
        Left = 0
        Top = 0
        Width = 526
        Height = 212
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object edtIgnoredUnits: TEdit
        Left = 8
        Top = 219
        Width = 264
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
      end
      object Button6: TButton
        Left = 282
        Top = 219
        Width = 75
        Height = 25
        Action = actIgnoredUnits_Add
        Anchors = [akRight, akBottom]
        TabOrder = 2
      end
      object Button7: TButton
        Left = 362
        Top = 219
        Width = 75
        Height = 25
        Action = actIgnoredUnits_Delete
        Anchors = [akRight, akBottom]
        TabOrder = 3
      end
      object Button10: TButton
        Left = 442
        Top = 219
        Width = 75
        Height = 25
        Action = actIgnoredUnits_Load
        Anchors = [akRight, akBottom]
        TabOrder = 4
      end
    end
    object tshRegisteredClasses: TTabSheet
      Caption = 'Registered Classes'
      ImageIndex = 5
      object lsbRegisteredClasses: TListBox
        Left = 0
        Top = 0
        Width = 526
        Height = 212
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object edtRegisteredClasses: TEdit
        Left = 8
        Top = 219
        Width = 264
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
      end
      object Button8: TButton
        Left = 282
        Top = 219
        Width = 75
        Height = 25
        Action = actRegisteredClasses_Add
        Anchors = [akRight, akBottom]
        TabOrder = 2
      end
      object Button9: TButton
        Left = 362
        Top = 219
        Width = 75
        Height = 25
        Action = actRegisteredClasses_Delete
        Anchors = [akRight, akBottom]
        TabOrder = 3
      end
      object Button11: TButton
        Left = 442
        Top = 219
        Width = 75
        Height = 25
        Action = actRegisteredClasses_Load
        Anchors = [akRight, akBottom]
        TabOrder = 4
      end
    end
  end
  object btnCancel: TButton
    Left = 469
    Top = 300
    Width = 75
    Height = 25
    Action = actCancel
    Anchors = [akRight, akBottom]
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 381
    Top = 300
    Width = 75
    Height = 25
    Action = actOK
    Anchors = [akRight, akBottom]
    TabOrder = 2
  end
  object btnApply: TButton
    Left = 293
    Top = 300
    Width = 75
    Height = 25
    Action = actApply
    Anchors = [akRight, akBottom]
    TabOrder = 3
  end
  object ActionList1: TActionList
    Left = 16
    Top = 296
    object actOK: TAction
      Caption = 'OK'
      OnExecute = actOKExecute
    end
    object actCancel: TAction
      Caption = 'Cancel'
      OnExecute = actCancelExecute
    end
    object actApply: TAction
      Caption = '&Apply'
      OnExecute = actApplyExecute
    end
    object actAdd: TAction
      Caption = '&Add'
      OnExecute = actAddExecute
      OnUpdate = actAddUpdate
    end
    object actDelete: TAction
      Caption = '&Delete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actAddNiceName: TAction
      Caption = '&Add'
      OnExecute = actAddNiceNameExecute
    end
    object actDeleteNiceName: TAction
      Caption = '&Delete'
      OnExecute = actDeleteNiceNameExecute
    end
    object actEditNiceName: TAction
      Caption = '&Edit'
      OnExecute = actEditNiceNameExecute
    end
    object actAddDirective: TAction
      Caption = '&Add'
      OnExecute = actAddDirectiveExecute
      OnUpdate = actAddDirectiveUpdate
    end
    object actDeleteDirective: TAction
      Caption = '&Delete'
      OnExecute = actDeleteDirectiveExecute
      OnUpdate = actDeleteDirectiveUpdate
    end
    object actEditDirective: TAction
      Caption = 'actEditDirective'
    end
    object actRegisteredClasses_Add: TAction
      Category = 'RegisteredClasses'
      Caption = 'Add'
      OnExecute = actRegisteredClasses_AddExecute
    end
    object actRegisteredClasses_Delete: TAction
      Category = 'RegisteredClasses'
      Caption = 'Delete'
      OnExecute = actRegisteredClasses_DeleteExecute
    end
    object actIgnoredUnits_Add: TAction
      Category = 'IgnoredUnits'
      Caption = 'Add'
      OnExecute = actIgnoredUnits_AddExecute
    end
    object actIgnoredUnits_Delete: TAction
      Category = 'IgnoredUnits'
      Caption = 'Delete'
      OnExecute = actIgnoredUnits_DeleteExecute
    end
    object actRegisteredClasses_Load: TAction
      Category = 'RegisteredClasses'
      Caption = 'Load'
      OnExecute = actRegisteredClasses_LoadExecute
    end
    object actIgnoredUnits_Load: TAction
      Category = 'IgnoredUnits'
      Caption = 'Load'
      OnExecute = actIgnoredUnits_LoadExecute
    end
    object actDocumentedUnits_Add: TAction
      Category = 'DocumentedUnits'
      Caption = 'Add'
    end
    object actDocumentedUnits_Delete: TAction
      Category = 'DocumentedUnits'
      Caption = 'Delete'
    end
    object actDocumentedUnits_Load: TAction
      Category = 'DocumentedUnits'
      Caption = 'Load'
    end
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 64
    Top = 296
  end
end
