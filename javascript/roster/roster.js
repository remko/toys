const React = require('react');
const ReactDOM = require('react-dom');
const FlipMove = require('react-flip-move');
const _ = require('lodash');
const { Strophe, $pres, $iq } = require('strophe');
const update = require('react-addons-update');
const md5 = require('blueimp-md5');
require('./roster.css');

// To work around strophe swallowing errors
const safeCB = (cb) => {
  return (...args) => {
    try { return cb(...args); }
    catch (e){ console.error('ERROR: ' + (e.stack ? e.stack : e)); }
    return true;
  };
};

const statuses = [ "online", "away", "xa", "dnd", "offline" ];

const RosterItem = React.createClass({
  render() {
    const { item } = this.props;
    return (
      <li className={item.activePresence.show}>
        <img className='avatar' src={`http://www.gravatar.com/avatar/${md5(_.toLower(item.jid))}`}/>
        {item.name || item.jid} { item.activePresence.status && `(${item.activePresence.status})`}
      </li>
    );
  }
});

const RosterGroup = React.createClass({
  render() {
    const groupItems = _(this.props.items)
        .sortBy(item => statuses.indexOf(item.activePresence.show), "name")
        .map(item => <RosterItem key={item.jid} item={item}/>)
        .value();
    return (
      <li className="group">
        <span className='group-title'>{this.props.name}</span>
        <ul className='roster-items'>
          <FlipMove>
              {groupItems}
          </FlipMove>
        </ul>
      </li>
    );
  }
});

const Roster = React.createClass({
  render() {
    const { items, presences, showOffline } = this.props;
    const itemsWithPresences = _(items)
      .map(item => {
        const itemPresences = _.values(presences[item.jid] || []);
        const activePresence = _.minBy(itemPresences, p => statuses.indexOf(p.show));
        return _.extend(item, { 
          presences: itemPresences, 
          activePresence: activePresence || { show: "offline" }
        });
      })
      .filter(item => showOffline || item.activePresence.show !== "offline")
      .value();
    const allGroupNames = _.uniq(_.flatten(_.map(items, "groups"))).sort();
    const allGroups = allGroupNames.map(groupName => {
      const groupItems = _.filter(itemsWithPresences, item => _.includes(item.groups, groupName));
      return <RosterGroup key={groupName} name={groupName} items={groupItems}/>;
    });
    const itemsWithoutGroup = _.filter(itemsWithPresences, item => _.isEmpty(item.groups));
    if (!_.isEmpty(itemsWithoutGroup)) {
      allGroups.unshift(<RosterGroup key="__noGroup" name="Contacts" items={itemsWithoutGroup}/>);
    }
    return <ul className='roster-groups'>{allGroups}</ul>;
  }
});


const LoginForm = React.createClass({
  render() {
    return (
      <form>
        <label>JID:</label>
        <input type='text' ref={el => { this.jid = el;}}/>
        <label>Password:</label>
        <input type='password' ref={el => { this.password = el; }}/>
        <input type='button' value='Connect'
          onClick={() => { this.props.onConnect(this.jid.value, this.password.value); } }/>
      </form>
    );
  }
});

const App = React.createClass({
  getInitialState() {
    return {
      roster: undefined,
      showOffline: false,
      presence: {},
      status: "disconnected"
    };
  },

  handleConnect(jid, password) {
    this.setState({ status: "connecting", error: undefined, presences: {}, roster: undefined });
    const connection = this.connection = new Strophe.Connection(`https://${Strophe.getDomainFromJid(jid)}/http-bind`);
    // connection.rawInput = (data) => { console.log('RECV: ' + data); };
    // connection.rawOutput = (data) => { console.log('SEND: ' + data); };
    connection.connect(jid, password, safeCB(status => {
      if (status === Strophe.Status.CONNECTING) {
        this.setState({ status: "connecting" });
      }
      else if (status === Strophe.Status.CONNFAIL) {
        this.setState({
          status: "disconnected",
          error: "failed to connect"
        });
      }
      else if (status === Strophe.Status.DISCONNECTING) {
        this.setState({ status: "disconnecting" });
      } 
      else if (status === Strophe.Status.DISCONNECTED) {
        this.setState({ status: "disconnected" });
      }
      else if (status === Strophe.Status.CONNECTED) {
        this.setState({ status: "connected" });
        connection.addHandler(safeCB(presenceEl => {
          const showEl = presenceEl.querySelector('show');
          let show = 'online';
          if (presenceEl.getAttribute('type') === 'unavailable') {
            show = 'offline';
          }
          else if (showEl) {
            show = showEl.textContent;
          }
          const statusEl = presenceEl.querySelector('status');
          const bareJID = Strophe.getBareJidFromJid(presenceEl.getAttribute('from'));
          const presence = {
            jid: presenceEl.getAttribute('from'),
            show,
            status: statusEl ? statusEl.textContent : undefined
          };
          if (this.state.presences.bareJID) {
            this.setState({ presences: update(this.state.presences, { 
              [bareJID]: { [presence.jid]: { $set: presence } }
            })});
          }
          else {
            this.setState({ presences: update(this.state.presences, { 
              [bareJID]: { $set: { [presence.jid]: presence } }
            })});
          }
          return true;
        }), null, 'presence', null, null, null); 
        connection.send($pres().c('priority', -1).tree());
        connection.sendIQ(
          $iq({type: 'get'}).c('query', {xmlns: 'jabber:iq:roster'}),
          safeCB(roster => {
            this.setState({
              roster: _.map(roster.querySelectorAll('item'), item => {
                return {
                  jid: item.getAttribute('jid'),
                  name: item.getAttribute('name'),
                  groups: _.map(item.querySelectorAll('group'), group => 
                    group.textContent)
                };
              })
            });
          })
        );
      }
      else {
        this.setState({status: "Unknown status: " + status });
      }
    }));
  },

  render() {
    if (this.state.status === "disconnected") {
      return (
        <div>
          {this.state.error && <div>{this.state.error}</div>}
          <LoginForm onConnect={this.handleConnect}/>
        </div>
      );
    }
    else if (this.state.roster) {
      return (
        <div>
          <label>
            <input 
              type='checkbox' 
              checked={this.state.showOffline}
              onChange={v => this.setState({showOffline: v.target.checked})}/>
            Show offline
          </label>
          <input type='button' value='Disconnect'
            onClick={() => this.connection.disconnect() } />
          <Roster 
            items={this.state.roster} 
            presences={this.state.presences}
            showOffline={this.state.showOffline}/>
        </div>
      );
    }
    else {
      return <div>{this.state.status}</div>;
    }
  }
});

ReactDOM.render(<App/>, document.getElementById('content'));
