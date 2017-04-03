{% if m.acl.use.mod_link_check %}
	<li><a href="{% url admin_link_check %}" {% ifequal selected "admin_link_check" %}class="current"{% endifequal %}>{_ Link Check _}</a></li>
{% endif %}
