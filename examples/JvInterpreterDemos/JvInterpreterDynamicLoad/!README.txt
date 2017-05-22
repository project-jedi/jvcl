Демонстрация динамической загрузки интерпретатора
=================================================

Это демонстрационное приложение работает в Delphi 5 и 6.
Оно также может быть использовано в других версиях Delphi,
кроме 2, однако это требует незначительной модификации
исходного кода (см. условия {$IFDEF}).

Состав
------

1. MyLabel.pas - модуль с нашим компонентом;
2. MyLabelPackage.dpk - пакет с нашим компонентом
   (Design-time and Run-time);
3. JvInterpreter_MyLabel.pas - модуль с адаптером для нашего
   компонента к интерпретатору;
4. JvInterpreter_MyLabelPackage.dpk - пакет с адаптером
   для нашего компонента к интепретатору (Run-time);
5. ScriptForm.pas/ScriptForm.dfm - форма, в которой
   используется наш компонент;
6. DynamicLoad.dpr - проект, демонстрирующий
   динамическую загрузку пакета с нашим компонентом
   и запуск интерпретатора;
7. MainForm.pas/MainForm.dfm - модуль с формой для
   тестового проекта. выполняет всю основную работу.

Сборка
------
1. Компилируем пакет MyLabelPackage.dpk, можно также
   проинсталлировать его в дельфи. Добавляет компонент
   TMyLabel на закладку "JVCL".
2. Компилируем пакет JvInterpreter_MyLabelPackage.dpk,
   не инсталлируем.
3. Компилируем демо-проект DynamicLoad.dpr.
4. Помещаем пакеты rai5.bpl, raia5.bpl в каталог, где
   находится DynamicLoad.exe.

Запуск
------
1. Запускаем DynamicLoad.exe и нажимаем на все кнопочки.

Как это работает
----------------
Вся функциональность заключается в модуле MainForm.pas.
Функция "DynamicJvInterpreterRunFormModal"
  procedure DynamicJvInterpreterRunFormModal(const FileName: TFileName);
загружает пакет интерпретатора и запускает указанную форму на выполнение.
Функция LoadJvInterpreterPackage
  function LoadJvInterpreterPackage(const PackageFileName: TFileName; const UnitName: String): HModule;
загружает дополнительные пакеты.
Перед запуском нашей тестовой формы (ScriptForm.pas/ScriptForm.dfm)
мы сначала используя функцию "LoadJvInterpreterPackage" загружаем
наш пакет с нашим классом и регистрируем его в глобальном адаптере.

Для изучения более подробного устройства воспользуйтесь
исходными текстами.

Andrei Prygounkov,                     21 сентября 2001 г.
a.prygounkov@gmx.de
