������������ ������������ �������� ��������������
=================================================

��� ���������������� ���������� �������� � Delphi 5 � 6.
��� ����� ����� ���� ������������ � ������ ������� Delphi,
����� 2, ������ ��� ������� �������������� �����������
��������� ���� (��. ������� {$IFDEF}).

������
------

1. MyLabel.pas - ������ � ����� �����������;
2. MyLabelPackage.dpk - ����� � ����� �����������
   (Design-time and Run-time);
3. JvInterpreter_MyLabel.pas - ������ � ��������� ��� ������
   ���������� � ��������������;
4. JvInterpreter_MyLabelPackage.dpk - ����� � ���������
   ��� ������ ���������� � ������������� (Run-time);
5. ScriptForm.pas/ScriptForm.dfm - �����, � �������
   ������������ ��� ���������;
6. DynamicLoad.dpr - ������, ���������������
   ������������ �������� ������ � ����� �����������
   � ������ ��������������;
7. MainForm.pas/MainForm.dfm - ������ � ������ ���
   ��������� �������. ��������� ��� �������� ������.

������
------
1. ����������� ����� MyLabelPackage.dpk, ����� �����
   ����������������� ��� � ������. ��������� ���������
   TMyLabel �� �������� "JVCL".
2. ����������� ����� JvInterpreter_MyLabelPackage.dpk,
   �� ������������.
3. ����������� ����-������ DynamicLoad.dpr.
4. �������� ������ rai5.bpl, raia5.bpl � �������, ���
   ��������� DynamicLoad.exe.

������
------
1. ��������� DynamicLoad.exe � �������� �� ��� ��������.

��� ��� ��������
----------------
��� ���������������� ����������� � ������ MainForm.pas.
������� "DynamicJvInterpreterRunFormModal"
  procedure DynamicJvInterpreterRunFormModal(const FileName: TFileName);
��������� ����� �������������� � ��������� ��������� ����� �� ����������.
������� LoadJvInterpreterPackage
  function LoadJvInterpreterPackage(const PackageFileName: TFileName; const UnitName: String): HModule;
��������� �������������� ������.
����� �������� ����� �������� ����� (ScriptForm.pas/ScriptForm.dfm)
�� ������� ��������� ������� "LoadJvInterpreterPackage" ���������
��� ����� � ����� ������� � ������������ ��� � ���������� ��������.

��� �������� ����� ���������� ���������� ��������������
��������� ��������.

Andrei Prygounkov,                     21 �������� 2001 �.
a.prygounkov@gmx.de
