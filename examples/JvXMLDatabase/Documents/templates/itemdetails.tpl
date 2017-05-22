

<tr><td align="left">

  <form action="item_detailschange.xhs">
  <input type="hidden" name="id" value="%ItemId%" />

  <table>

    <tr><td><b>Nom:</b></td><td> <input type="text" name="name" value="%ItemName%" /> </td></tr>
    <tr><td><b>Status:</b></td><td>
      <select name="status">
        <option value="%ItemStatus%">Actuel: %StatName%</option>
        <option value="%ItemStatus%"></option>
        <option value="1">Disponible</option>
        <option value="2">Dans une boîte</option>
        <option value="3">En stérilisation</option>
        <option value="4">Perdu</option>
      </select></td></tr>

    <tr><td><b><a href="categories.xhs">Catégorie</a>:</b></td><td>
      <select name="category">
        <option value="%CatId%">Actuel: %CatName%</option>
        <option value="%CatId%"></option>

