

<form action="box_detailschange.xhs">
<input type="hidden" name="id" value="%BoxId%" />

<table>

<tr><td><b>Nom:</b></td><td> <input type="text" name="name" value="%BoxName%" /></td></tr>
<tr><td><b>Status:</b></td><td>
  <select name="status">
    <option value="%StatId%">Actuel: %StatName%</option>
    <option value="%StatId%"></option>
    <option value="1">Disponible</option>
    <option value="2">En stérilisation</option>
    <option value="3">Empruntée</option>
    <option value="4">Supprimée</option>
  </select></td></tr>
<tr><td><b>Peremption:</b></td><td> <input type="text" name="peremption" value="%BoxPeremption%" /></td></tr>

<tr><td></td><td><input type="submit" value="Sauver" /></td></tr>

</table>
</form>

